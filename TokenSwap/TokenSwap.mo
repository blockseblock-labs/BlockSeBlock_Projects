import Map "mo:base/OrderedMap";
import Trie "mo:base/Trie";
import Float "mo:base/Float";
import Principal "mo:base/Principal";
import Text "mo:base/Text";
import Debug "mo:base/Debug";

persistent actor TokenSwap {
    type Token = Principal;
    type PoolId = Text;
    type User = Principal;
    type Balance = Float;
    type Slippage = Float;
    type ExchangeRate = Float;
    type FeeRate = Float;

    type Pool = {
        tokenA : Token;
        tokenB : Token;
        balanceA : Balance;
        balanceB : Balance;
    };

    type Balances = Trie.Trie<Token, Balance>;

    transient let poolMap = Map.Make<PoolId>(Text.compare);
    var pools : Map.Map<PoolId, Pool> = poolMap.empty();

    var userBalances : Trie.Trie<User, Balances> = Trie.empty();
    var feeRate : FeeRate = 0.003; // 0.3%
    var admin : ?Principal = null;

    public shared ({ caller }) func initializeAdmin() : async () {
        if (admin != null) {
            Debug.trap("Admin already initialized");
        };
        admin := ?caller;
    };

    // Add liquidity to a pool
    public shared func addLiquidity(poolId : PoolId, tokenA : Token, tokenB : Token, amountA : Balance, amountB : Balance) : async () {
        switch (poolMap.get(pools, poolId)) {
            case null {
                pools := poolMap.put(
                    pools,
                    poolId,
                    {
                        tokenA = tokenA;
                        tokenB = tokenB;
                        balanceA = amountA;
                        balanceB = amountB;
                    },
                );
            };
            case (?pool) {
                pools := poolMap.put(
                    pools,
                    poolId,
                    {
                        tokenA = pool.tokenA;
                        tokenB = pool.tokenB;
                        balanceA = pool.balanceA + amountA;
                        balanceB = pool.balanceB + amountB;
                    },
                );
            };
        };
    };

    // Remove liquidity from a pool
    public shared func removeLiquidity(poolId : PoolId, amountA : Balance, amountB : Balance) : async () {
        switch (poolMap.get(pools, poolId)) {
            case null { Debug.trap("Pool not found") };
            case (?pool) {
                if (pool.balanceA < amountA or pool.balanceB < amountB) {
                    Debug.trap("Insufficient liquidity");
                };
                pools := poolMap.put(
                    pools,
                    poolId,
                    {
                        tokenA = pool.tokenA;
                        tokenB = pool.tokenB;
                        balanceA = pool.balanceA - amountA;
                        balanceB = pool.balanceB - amountB;
                    },
                );
            };
        };
    };

    // Swap tokens with slippage control
    public shared func swapTokens(poolId : PoolId, fromToken : Token, toToken : Token, amount : Balance, slippage : Slippage) : async Balance {
        switch (poolMap.get(pools, poolId)) {
            case null { Debug.trap("Pool not found") };
            case (?pool) {
                if (pool.tokenA != fromToken or pool.tokenB != toToken) {
                    Debug.trap("Invalid token pair");
                };

                let exchangeRate = pool.balanceB / pool.balanceA;
                let expectedAmount = amount * exchangeRate;
                let minAmount = expectedAmount * (1.0 - slippage);

                if (pool.balanceB < minAmount) {
                    Debug.trap("Insufficient liquidity for swap");
                };

                pools := poolMap.put(
                    pools,
                    poolId,
                    {
                        tokenA = pool.tokenA;
                        tokenB = pool.tokenB;
                        balanceA = pool.balanceA + amount;
                        balanceB = pool.balanceB - minAmount;
                    },
                );

                minAmount;
            };
        };
    };

    // Fetch token balances for a user
    public shared query func getBalance(user : User, token : Token) : async Balance {
        let userKey = { key = user; hash = Principal.hash(user) };
        switch (Trie.find(userBalances, userKey, Principal.equal)) {
            case null { 0 };
            case (?balances) {
                let tokenKey = { key = token; hash = Principal.hash(token) };
                switch (Trie.find(balances, tokenKey, Principal.equal)) {
                    case null { 0 };
                    case (?balance) { balance };
                };
            };
        };
    };

    // Evaluate exchange rates for token pairs
    public shared query func getExchangeRate(poolId : PoolId) : async ExchangeRate {
        switch (poolMap.get(pools, poolId)) {
            case null { Debug.trap("Pool not found") };
            case (?pool) { pool.balanceB / pool.balanceA };
        };
    };

    // Manage trading fees
    public shared ({ caller }) func setFeeRate(newFeeRate : FeeRate) : async () {
        switch (admin) {
            case (?adminPrincipal) if (caller != adminPrincipal) Debug.trap("Unauthorized");
            case (null) Debug.trap("Admin not initialized");
        };
        feeRate := newFeeRate;
    };
};
