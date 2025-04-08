import Nat "mo:base/Nat";
import Principal "mo:base/Principal";
import Text "mo:base/Text";

persistent actor AmmLiquidity {
    type Pool = {
        tokenAReserve : Nat;
        tokenBReserve : Nat;
        totalLiquidity : Nat;
    };
    var pool : Pool = {
        tokenAReserve = 0;
        tokenBReserve = 0;
        totalLiquidity = 0;
    };
    type RebalanceResponse = {
        #successful;
        #notRequired;
        #unAuthorized;
    };
    type TokenSwap = {
        #invalid;
        #swappedAforB;
        #swappedBforA;
    };
    type Liquidity = {
        #insufficient;
        #successful;
    };
    var admin : ?Principal = null;
    public shared ({ caller }) func initDeployer() : async Bool {
        if (admin == null) { admin := ?caller; true } else false;
    };
    public func addLiquidity(tokenA : Nat, tokenB : Nat) : async () {
        pool := {
            tokenAReserve = pool.tokenAReserve + tokenA;
            tokenBReserve = pool.tokenBReserve + tokenB;
            totalLiquidity = pool.totalLiquidity + Nat.min(tokenA, tokenB);
        };
    };
    public func removeLiquidity(amount : Nat) : async Liquidity {
        if (amount > pool.totalLiquidity) {
            return #insufficient;
        };
        let tokenA_amount = (amount * pool.tokenAReserve) / pool.totalLiquidity;
        let tokenB_amount = (amount * pool.tokenBReserve) / pool.totalLiquidity;
        pool := {
            tokenAReserve = pool.tokenAReserve - tokenA_amount;
            tokenBReserve = pool.tokenBReserve - tokenB_amount;
            totalLiquidity = pool.totalLiquidity - Nat.min(tokenA_amount, tokenB_amount);
        };
        #successful;
    };

    public func swap(tokenIn : Text, amount : Nat) : async TokenSwap {
        if (tokenIn == "TokenA") {
            let amountOut = (amount * pool.tokenBReserve) / (pool.tokenAReserve + amount);
            pool := {
                tokenAReserve = pool.tokenAReserve + amount;
                tokenBReserve = pool.tokenBReserve - amountOut;
                totalLiquidity = pool.totalLiquidity;
            };
            #swappedAforB;
        } else if (tokenIn == "TokenB") {
            let amountOut = (amount * pool.tokenAReserve) / (pool.tokenBReserve + amount);
            pool := {
                tokenAReserve = pool.tokenBReserve + amount;
                tokenBReserve = pool.tokenAReserve - amountOut;
                totalLiquidity = pool.totalLiquidity;
            };
            #swappedAforB;
        } else #invalid;
    };

    public shared ({ caller }) func rebalance() : async RebalanceResponse {
        if (admin != ?caller) return #unAuthorized;
        let optimal_ratio = 1;
        let current_ratio = pool.tokenAReserve / pool.tokenBReserve;
        if (Nat.sub(optimal_ratio, current_ratio) > 5) {
            pool := {
                tokenAReserve = (pool.tokenAReserve + pool.tokenBReserve) / 2;
                tokenBReserve = (pool.tokenAReserve + pool.tokenBReserve) / 2;
                totalLiquidity = pool.totalLiquidity;
            };
            #successful;
        } else #notRequired;
    };

    public query func getPoolState() : async Pool {
        pool;
    };
};