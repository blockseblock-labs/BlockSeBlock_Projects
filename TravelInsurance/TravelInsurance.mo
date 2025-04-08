import Nat "mo:base/Nat";
import Text "mo:base/Text";
import List "mo:base/List";
import Time "mo:base/Time";
import Trie "mo:base/Trie";
import Principal "mo:base/Principal";
import Result "mo:base/Result";
import Array "mo:base/Array";

persistent actor TravelInsurance {
    type PolicyId = Nat;
    type CustomerId = Principal;

    type Coverage = {
        #medical;
        #cancellation;
        #baggage;
        #accident;
        #all;
    };

    type PolicyStatus = {
        #active;
        #expired;
        #cancelled;
        #claimed;
    };

    type InsurancePolicy = {
        id : PolicyId;
        customerId : CustomerId;
        destination : Text;
        startDate : Time.Time;
        endDate : Time.Time;
        coverage : Coverage;
        premium : Nat;
        status : PolicyStatus;
    };

    type Claim = {
        policyId : PolicyId;
        description : Text;
        amount : Nat;
        status : ClaimStatus;
        timestamp : Time.Time;
    };

    type ClaimStatus = {
        #pending;
        #approved;
        #rejected;
    };

    private var nextPolicyId : Nat = 1;
    private var adminPrincipal : Principal = Principal.fromText("aaaaa-aa");
    private var claims : [Claim] = [];
    private var policies : Trie.Trie<PolicyId, InsurancePolicy> = Trie.empty();
    private var customerPolicies : Trie.Trie<CustomerId, List.List<PolicyId>> = Trie.empty();
    private var activePolicySet : Trie.Trie<PolicyId, ()> = Trie.empty();

    private func isAdmin(caller : Principal) : Bool {
        Principal.equal(caller, adminPrincipal);
    };

    private func requireAdmin(caller : Principal) : Result.Result<(), Text> {
        if (isAdmin(caller)) {
            #ok(());
        } else {
            #err("Unauthorized: Admin access required");
        };
    };

    public shared ({ caller }) func updateAdmin(newAdmin : Principal) : async Result.Result<(), Text> {
        switch (requireAdmin(caller)) {
            case (#err(e)) { #err(e) };
            case (#ok()) {
                adminPrincipal := newAdmin;
                #ok(());
            };
        };
    };

    private func key(n : Nat) : Trie.Key<Nat> {
        { key = n; hash = Text.hash(Nat.toText(n)) };
    };

    private func principalKey(p : Principal) : Trie.Key<Principal> {
        { key = p; hash = Text.hash(Principal.toText(p)) };
    };

    public shared (msg) func purchasePolicy(
        destination : Text,
        startDate : Time.Time,
        endDate : Time.Time,
        coverage : Coverage,
        premium : Nat,
    ) : async Result.Result<PolicyId, Text> {
        let customerId = msg.caller;

        if (startDate >= endDate) {
            return #err("Invalid date range");
        };

        let policy : InsurancePolicy = {
            id = nextPolicyId;
            customerId = customerId;
            destination = destination;
            startDate = startDate;
            endDate = endDate;
            coverage = coverage;
            premium = premium;
            status = #active;
        };

        policies := Trie.put(
            policies,
            key(nextPolicyId),
            Nat.equal,
            policy,
        ).0;

        let existingPolicies = switch (Trie.get(customerPolicies, principalKey(customerId), Principal.equal)) {
            case null List.nil<PolicyId>();
            case (?list) list;
        };

        customerPolicies := Trie.put(
            customerPolicies,
            principalKey(customerId),
            Principal.equal,
            List.push(nextPolicyId, existingPolicies),
        ).0;

        activePolicySet := Trie.put(
            activePolicySet,
            key(nextPolicyId),
            Nat.equal,
            (),
        ).0;

        let currentId = nextPolicyId;
        nextPolicyId += 1;

        #ok(currentId);
    };

    public shared (msg) func submitClaim(
        policyId : PolicyId,
        description : Text,
        amount : Nat,
    ) : async Result.Result<(), Text> {
        switch (Trie.get(policies, key(policyId), Nat.equal)) {
            case null {
                #err("Policy not found");
            };
            case (?policy) {
                if (policy.customerId != msg.caller) {
                    return #err("Not authorized");
                };

                if (policy.status != #active) {
                    return #err("Policy is not active");
                };

                let claim : Claim = {
                    policyId = policyId;
                    description = description;
                    amount = amount;
                    status = #pending;
                    timestamp = Time.now();
                };

                claims := Array.append(claims, [claim]);
                #ok(());
            };
        };
    };

    public shared ({ caller }) func processClaim(policyId : PolicyId, approved : Bool) : async Result.Result<(), Text> {
        switch (requireAdmin(caller)) {
            case (#err(e)) { #err(e) };
            case (#ok()) {
                let updatedClaims = Array.map<Claim, Claim>(
                    claims,
                    func(claim : Claim) : Claim {
                        if (claim.policyId == policyId) {
                            {
                                claim with
                                status = if (approved) #approved else #rejected;
                            };
                        } else {
                            claim;
                        };
                    },
                );

                claims := updatedClaims;
                #ok(());
            };
        };
    };

    public query func getPolicyDetails(policyId : PolicyId) : async Result.Result<InsurancePolicy, Text> {
        switch (Trie.get(policies, key(policyId), Nat.equal)) {
            case null #err("Policy not found");
            case (?policy) #ok(policy);
        };
    };

    public query ({ caller }) func isCallerAdmin() : async Bool {
        isAdmin(caller);
    };

    system func heartbeat() : async () {
        await checkExpiredPolicies();
    };

    private func checkExpiredPolicies() : async () {
        let currentTime = Time.now();

        for ((id, _) in Trie.iter(activePolicySet)) {
            switch (Trie.get(policies, key(id), Nat.equal)) {
                case null {};
                case (?policy) {
                    if (policy.endDate < currentTime) {
                        let updatedPolicy = {
                            policy with
                            status = #expired;
                        };

                        policies := Trie.put(
                            policies,
                            key(id),
                            Nat.equal,
                            updatedPolicy,
                        ).0;

                        activePolicySet := Trie.remove(
                            activePolicySet,
                            key(id),
                            Nat.equal,
                        ).0;
                    };
                };
            };
        };
    };
};