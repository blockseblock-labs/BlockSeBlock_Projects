import Map "mo:base/OrderedMap";
import Trie "mo:base/Trie";
import Float "mo:base/Float";
import Principal "mo:base/Principal";
import Text "mo:base/Text";
import Debug "mo:base/Debug";

persistent actor LiquidityPool {
    type Token = Principal;
    type PoolId = Text;
    type User = Principal;
    type Balance = Float;
    type FeeRate = Float;

    type Pool = {
        tokenA : Token;
        tokenB : Token;
        reserveA : Balance;
        reserveB : Balance;
        lpTokens : Balance;
    };

    public type UserBalances = Trie.Trie<User, Balance>;

    transient let poolMap = Map.Make<PoolId>(Text.compare);
    var pools : Map.Map<PoolId, Pool> = poolMap.empty();

    var lpBalances : Trie.Trie<PoolId, UserBalances> = Trie.empty();

    // stable var feeRate : FeeRate = 0.003; // 0.3%

    public shared ({ caller }) func addLiquidity(poolId : PoolId, tokenA : Token, tokenB : Token, amountA : Balance, amountB : Balance) : async Balance {
        switch (poolMap.get(pools, poolId)) {
            case null {
                let newPool : Pool = {
                    tokenA = tokenA;
                    tokenB = tokenB;
                    reserveA = amountA;
                    reserveB = amountB;
                    lpTokens = amountA + amountB;
                };
                pools := poolMap.put(pools, poolId, newPool);

                // Issue LP tokens to the user
                let userKey = { key = caller; hash = Principal.hash(caller) };
                let userBalances = Trie.find(lpBalances, { key = poolId; hash = Text.hash(poolId) }, Text.equal);
                let updatedUserBalances : UserBalances = switch (userBalances) {
                    case null {
                        Trie.put(Trie.empty(), userKey, Principal.equal, amountA + amountB).0;
                    };
                    case (?balances) {
                        Trie.put(balances, userKey, Principal.equal, amountA + amountB).0;
                    };
                };
                lpBalances := Trie.put(lpBalances, { key = poolId; hash = Text.hash(poolId) }, Text.equal, updatedUserBalances).0;

                return amountA + amountB;
            };
            case (?pool) {
                let lpTokensIssued : Balance = (amountA + amountB) * (pool.lpTokens / (pool.reserveA + pool.reserveB));
                let updatedPool : Pool = {
                    tokenA = pool.tokenA;
                    tokenB = pool.tokenB;
                    reserveA = pool.reserveA + amountA;
                    reserveB = pool.reserveB + amountB;
                    lpTokens = pool.lpTokens + lpTokensIssued;
                };
                pools := poolMap.replace(pools, poolId, updatedPool).0;

                // Issue LP tokens to the user
                let userKey = { key = caller; hash = Principal.hash(caller) };
                let userBalances = Trie.find(lpBalances, { key = poolId; hash = Text.hash(poolId) }, Text.equal);
                let updatedUserBalances = switch (userBalances) {
                    case null {
                        Trie.put(Trie.empty(), userKey, Principal.equal, lpTokensIssued).0;
                    };
                    case (?balances) {
                        let find = Trie.find<User, Balance>(balances, userKey, Principal.equal);
                        let userBalance = switch (find) {
                            case null { 0.0 };
                            case (?balance) { balance };
                        };
                        Trie.put(balances, userKey, Principal.equal, userBalance + lpTokensIssued).0;
                    };
                };
                lpBalances := Trie.put(lpBalances, { key = poolId; hash = Text.hash(poolId) }, Text.equal, updatedUserBalances).0;

                return lpTokensIssued;
            };
        };
    };

    // Remove liquidity from a pool
    public shared ({ caller }) func removeLiquidity(poolId : PoolId, lpTokens : Balance) : async (Balance, Balance) {
        switch (poolMap.get(pools, poolId)) {
            case null {
                Debug.trap("Pool not found");
            };
            case (?pool) {
                let userKey = { key = caller; hash = Principal.hash(caller) };
                let userBalances = Trie.find(lpBalances, { key = poolId; hash = Text.hash(poolId) }, Text.equal);
                switch (userBalances) {
                    case null {
                        Debug.trap("No LP tokens for user");
                    };
                    case (?balances) {
                        let find = Trie.find<User, Balance>(balances, userKey, Principal.equal);
                        let userBalance = switch (find) {
                            case null { 0.0 };
                            case (?balance) { balance };
                        };
                        if (userBalance < lpTokens) {
                            Debug.trap("Insufficient LP tokens");
                        };

                        let amountA = (lpTokens / pool.lpTokens) * pool.reserveA;
                        let amountB = (lpTokens / pool.lpTokens) * pool.reserveB;

                        let updatedPool : Pool = {
                            tokenA = pool.tokenA;
                            tokenB = pool.tokenB;
                            reserveA = pool.reserveA - amountA;
                            reserveB = pool.reserveB - amountB;
                            lpTokens = pool.lpTokens - lpTokens;
                        };
                        pools := poolMap.replace(pools, poolId, updatedPool).0;

                        // Update user's LP token balance
                        let updatedUserBalances = Trie.put(balances, userKey, Principal.equal, userBalance - lpTokens).0;
                        lpBalances := Trie.put(lpBalances, { key = poolId; hash = Text.hash(poolId) }, Text.equal, updatedUserBalances).0;

                        return (amountA, amountB);
                    };
                };
            };
        };
    };

    // Query pool reserves
    public shared query func getPoolReserves(poolId : PoolId) : async (Balance, Balance) {
        switch (poolMap.get(pools, poolId)) {
            case null {
                Debug.trap("Pool not found");
            };
            case (?pool) {
                return (pool.reserveA, pool.reserveB);
            };
        };
    };

    // Query LP token balance for a user
    public shared query func getLpBalance(user : User, poolId : PoolId) : async Balance {
        let userKey = { key = user; hash = Principal.hash(user) };
        let userBalances = Trie.find(lpBalances, { key = poolId; hash = Text.hash(poolId) }, Text.equal);
        switch (userBalances) {
            case null { return 0.0 };
            case (?balances) {
                let find = Trie.find<User, Balance>(balances, userKey, Principal.equal);
                switch (find) {
                    case null { return 0.0 };
                    case (?balance) { return balance };
                };
            };
        };
    };
};