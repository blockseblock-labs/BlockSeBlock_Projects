import Principal "mo:base/Principal";
import Nat "mo:base/Nat";
import Float "mo:base/Float";
import Map "mo:base/OrderedMap";

persistent actor CreditScoring {
    transient let userMap = Map.Make<Principal>(Principal.compare);
    var users : Map.Map<Principal, UserData> = userMap.empty<UserData>();

    type UserData = {
        borrowedAmount : Nat;
        repaidAmount : Nat;
        creditScore : Nat;
    };

    public shared (msg) func borrow(amount : Nat) : async Nat {
        let user = msg.caller;
        let existingUser = userMap.get(users, user);
        let updatedUserData : UserData = switch existingUser {
            case (?data) {
                {
                    borrowedAmount = data.borrowedAmount + amount;
                    repaidAmount = data.repaidAmount;
                    creditScore = data.creditScore;
                };
            };
            case null {
                {
                    borrowedAmount = amount;
                    repaidAmount = 0;
                    creditScore = 0;
                };
            };
        };
        users := userMap.put(users, user, updatedUserData);
        amount;
    };

    public shared (msg) func repay(amount : Nat) : async Nat {
        let user = msg.caller;
        switch (userMap.get(users, user)) {
            case null return 0;
            case (?userData) {
                let newRepaidAmount = userData.repaidAmount + amount;
                let updatedUserData : UserData = {
                    borrowedAmount = userData.borrowedAmount;
                    repaidAmount = newRepaidAmount;
                    creditScore = calculateCreditScore(userData.borrowedAmount, newRepaidAmount);
                };
                users := userMap.put(users, user, updatedUserData);
            };
        };
        amount;
    };

    func calculateCreditScore(borrowedAmount : Nat, repaidAmount : Nat) : Nat {
        if (borrowedAmount == 0) return 0;
        let score = (repaidAmount * 100) / borrowedAmount;
        score;
    };

    public query (msg) func getInterestRate() : async Float {
        let user = msg.caller;
        switch (userMap.get(users, user)) {
            case null 10.0;
            case (?userData) {
                let baseRate = 10.0;
                let discount = (Float.fromInt(userData.creditScore) / 100.0) * 5.0;
                baseRate - discount;
            };
        };
    };

    public query func getUserData(user : Principal) : async ?UserData {
        userMap.get(users, user);
    };
};