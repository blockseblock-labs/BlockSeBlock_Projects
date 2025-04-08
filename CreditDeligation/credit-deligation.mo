import Map "mo:base/OrderedMap";
import Principal "mo:base/Principal";
import Nat "mo:base/Nat";
import Int "mo:base/Int";
import Debug "mo:base/Debug";
import Time "mo:base/Time";
import Float "mo:base/Float";
import Array "mo:base/Array";

persistent actor CreditDelegation {

    public type DelegationId = Nat;
    public type CreditLimit = Nat;

    public type CreditDelegator = {
        delegator : Principal;
        totalCreditLimit : CreditLimit;
        availableCreditLimit : CreditLimit;
        delegationFeeRate : Float; // Percentage fee for delegation
        activeDelegations : [DelegationId];
    };

    public type CreditDelegation = {
        id : DelegationId;
        delegator : Principal;
        delegatee : Principal;
        creditLimit : CreditLimit;
        status : { #active; #revoked };
        delegationFee : Nat;
    };

    var nextDelegationId : DelegationId = 0;

    transient let delegatorMap = Map.Make<Principal>(Principal.compare);
    var delegators : Map.Map<Principal, CreditDelegator> = delegatorMap.empty();

    transient let delegationMap = Map.Make<DelegationId>(Nat.compare);
    var delegations : Map.Map<DelegationId, CreditDelegation> = delegationMap.empty();

    let DEFAULT_DELEGATION_FEE_RATE : Float = 0.01; // 1% default delegation fee
    let MAX_DELEGATION_PERIOD : Time.Time = 365 * 24 * 60 * 60 * 1_000_000_000; // 1 year
    let MIN_DELEGATION_AMOUNT : CreditLimit = 100; // Minimum credit delegation amount

    // Register as a credit delegator
    public shared ({ caller }) func registerAsDelegator(totalCreditLimit : CreditLimit) : async () {
        if (totalCreditLimit < MIN_DELEGATION_AMOUNT) {
            Debug.trap("Credit limit too low");
        };

        let newDelegator : CreditDelegator = {
            delegator = caller;
            totalCreditLimit;
            availableCreditLimit = totalCreditLimit;
            delegationFeeRate = DEFAULT_DELEGATION_FEE_RATE;
            activeDelegations = [];
        };

        delegators := delegatorMap.put(delegators, caller, newDelegator);
    };

    // Delegate credit to another user
    public shared ({ caller }) func delegateCredit(
        delegatee : Principal,
        creditLimit : CreditLimit,
        delegationPeriod : Time.Time,
    ) : async DelegationId {
        switch (delegatorMap.get(delegators, caller)) {
            case (null) Debug.trap("Not registered as delegator");
            case (?delegator) {
                if (creditLimit > delegator.availableCreditLimit) {
                    Debug.trap("Insufficient available credit limit");
                };

                if (delegationPeriod > MAX_DELEGATION_PERIOD) {
                    Debug.trap("Delegation period too long");
                };

                let delegationFee = Int.abs(Float.toInt(Float.fromInt(creditLimit) * delegator.delegationFeeRate));

                let delegationId = nextDelegationId;
                nextDelegationId += 1;

                let newDelegation : CreditDelegation = {
                    id = delegationId;
                    delegator = caller;
                    delegatee;
                    creditLimit;
                    status = #active;
                    delegationFee = delegationFee;
                };

                let updatedDelegator : CreditDelegator = {
                    delegator with
                    availableCreditLimit : Nat = delegator.availableCreditLimit - creditLimit;
                    activeDelegations = Array.append(delegator.activeDelegations, [delegationId]);
                };

                delegations := delegationMap.put(delegations, delegationId, newDelegation);
                delegators := delegatorMap.put(delegators, caller, updatedDelegator);

                delegationId;
            };
        };
    };

    // Revoke credit delegation
    public shared ({ caller }) func revokeDelegation(delegationId : DelegationId) : async () {
        switch (delegationMap.get(delegations, delegationId)) {
            case (null) Debug.trap("Delegation not found");
            case (?delegation) {
                if (delegation.delegator != caller) {
                    Debug.trap("Not authorized to revoke this delegation");
                };

                let revokedDelegation : CreditDelegation = {
                    delegation with status = #revoked;
                };

                switch (delegatorMap.get(delegators, caller)) {
                    case (null) Debug.trap("Delegator not found");
                    case (?delegator) {
                        let updatedDelegator : CreditDelegator = {
                            delegator with
                            availableCreditLimit = delegator.availableCreditLimit + delegation.creditLimit;
                        };

                        delegations := delegationMap.put(delegations, delegationId, revokedDelegation);
                        delegators := delegatorMap.put(delegators, caller, updatedDelegator);
                    };
                };
            };
        };
    };

    // Query delegated credit details
    public query func getDelegation(delegationId : DelegationId) : async CreditDelegation {
        switch (delegationMap.get(delegations, delegationId)) {
            case (null) Debug.trap("Delegation not found");
            case (?delegation) delegation;
        };
    };

    // Query delegator's credit details
    public query func getDelegatorDetails(delegator : Principal) : async CreditDelegator {
        switch (delegatorMap.get(delegators, delegator)) {
            case (null) Debug.trap("Delegator not found");
            case (?details) details;
        };
    };

    // Set custom delegation fee rate
    public shared ({ caller }) func setDelegationFeeRate(feeRate : Float) : async () {
        if (feeRate < 0 or feeRate > 0.5) {
            Debug.trap("Invalid fee rate");
        };

        switch (delegatorMap.get(delegators, caller)) {
            case (null) Debug.trap("Not registered as delegator");
            case (?delegator) {
                let updatedDelegator : CreditDelegator = {
                    delegator with delegationFeeRate = feeRate;
                };
                delegators := delegatorMap.put(delegators, caller, updatedDelegator);
            };
        };
    };

    // Calculate total delegated credit for a user
    public query func calculateTotalDelegatedCredit(user : Principal) : async Nat {
        var totalDelegatedCredit : Nat = 0;
        for (delegation in delegationMap.vals(delegations)) {
            if (delegation.delegatee == user and delegation.status == #active) {
                totalDelegatedCredit += delegation.creditLimit;
            };
        };
        totalDelegatedCredit;
    };

    // List all active delegations for a delegator
    public query func listDelegations(delegator : Principal) : async [DelegationId] {
        switch (delegatorMap.get(delegators, delegator)) {
            case (null) [];
            case (?details) details.activeDelegations;
        };
    };

};