import Nat "mo:base/Nat";
import Principal "mo:base/Principal";
import Float "mo:base/Float";

persistent actor FractionalReserve {
    type Balance = Nat;
    type Price = Float;
    var totalSupply : Balance = 1_000_000;
    var reserve : Balance = 500_000;
    var reserveRatio : Float = 0.5;
    var expansionThreshold : Price = 1.05;
    var contractionThreshold : Price = 0.95;
    private var admin : ?Principal = null;
    private var priceOracle : ?Principal = null;

    type SupplyAdjustment = {
        #increased;
        #decreased;
        #stableSupply;
        #insufficientReserves;
        #unauthorized;
    };
    type ReserveRatio = {
        #updated;
        #invalid;
        #unauthorized;
    };
    public shared ({ caller }) func tryInitializeAdmin() : async Bool {
        if (admin == null) {
            admin := ?caller;
            true;
        } else false;
    };

    public shared ({ caller }) func setOracle(oracle : Principal) : async Bool {
        if (admin == ?caller) {
            priceOracle := ?oracle;
            true;
        } else false;
    };

    public shared ({ caller }) func updatePrice(newPrice : Price) : async SupplyAdjustment {
        if (priceOracle != ?caller) return #unauthorized;
        if (newPrice > expansionThreshold) {
            let expansionAmount = Nat.div(totalSupply, 10);
            totalSupply += expansionAmount;
            reserve += Nat.div(expansionAmount, 2);
            #increased;
        } else if (newPrice < contractionThreshold) {
            let contractionAmount = Nat.div(totalSupply, 10);
            if (reserve >= Nat.div(contractionAmount, 2)) {
                totalSupply -= contractionAmount;
                reserve -= Nat.div(contractionAmount, 2);
                #decreased;
            } else #insufficientReserves;
        } else #stableSupply;
    };

    public shared ({ caller }) func adjustReserveRatio(newRatio : Float) : async ReserveRatio {
        if (admin == ?caller) {
            if (newRatio >= 0.1 and newRatio <= 1.0) {
                reserveRatio := newRatio;
                return #updated;
            } else #invalid;
        } else #unauthorized;
    };

    public query func getTotalSupply() : async Balance {
        totalSupply;
    };

    public query func getReserveBalance() : async Balance {
        reserve;
    };

    public query func getReserveRatio() : async Float {
        reserveRatio;
    };
};