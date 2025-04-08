import OrderedMap "mo:base/OrderedMap";
import Principal "mo:base/Principal";
import List "mo:base/List";
import Float "mo:base/Float";
import Iter "mo:base/Iter";

persistent actor Expenses {
    // Create a custom comparison function for (Principal, Principal)
    func comparePrincipalPair(a : (Principal, Principal), b : (Principal, Principal)) : {
        #less;
        #equal;
        #greater;
    } {
        switch (Principal.compare(a.0, b.0)) {
            case (#less) { #less };
            case (#greater) { #greater };
            case (#equal) {
                Principal.compare(a.1, b.1);
            };
        };
    };
    // Instantiate the principalPairMap operations
    private transient let principalPairMap = OrderedMap.Make<(Principal, Principal)>(comparePrincipalPair);

    // the actual data
    private var expenses : OrderedMap.Map<(Principal, Principal), List.List<Float>> = principalPairMap.empty<List.List<Float>>();

    private func addToExpenses(key : (Principal, Principal), amount : Float) {
        let existingEntry : ?List.List<Float> = principalPairMap.get(expenses, key);
        let newEntry = switch (existingEntry) {
            case null { ?(amount, null) };
            case (?xs) { List.push(amount, xs) };
        };
        expenses := principalPairMap.put(expenses, key, newEntry);
    };

    public shared (msg) func addExpense(amount : Float, sharedWith : List.List<Principal>) : async () {
        let caller = msg.caller;
        let amountPerPerson = amount / Float.fromInt((1 + List.size(sharedWith)));
        addToExpenses((caller, caller), amountPerPerson);
        for (other in Iter.fromList(sharedWith)) {
            addToExpenses((caller, other), amountPerPerson);
        };
    };

    /// Get the total of expenses recorded by the caller.
    public shared (msg) func getExpensesTotal() : async Float {
        func f(k : (Principal, Principal), v : List.List<Float>) : ?Float {
            if (k.0 == msg.caller) {
                ?List.foldLeft(v, 0 : Float, Float.add);
            } else {
                null;
            };
        };
        let all = Iter.toList(principalPairMap.vals(principalPairMap.mapFilter(expenses, f)));
        List.foldLeft(all, 0 : Float, Float.add);
    };

    /// For each principal the caller owes, return the total amount owed.
    public shared (msg) func getDebts() : async List.List<(Principal, Float)> {
        var result : List.List<(Principal, Float)> = null;
        for (((giver, receiver), amounts) in principalPairMap.entries(expenses)) {
            if (msg.caller == receiver and msg.caller != giver) {
                result := List.push((giver, List.foldLeft(amounts, 0 : Float, Float.add)), result);
            };
        };
        result;
    };

    /// For each principal that owes the caller, return the total amount owed.
    public shared (msg) func getDebtors() : async List.List<(Principal, Float)> {
        var result : List.List<(Principal, Float)> = null;
        for (((giver, receiver), amounts) in principalPairMap.entries(expenses)) {
            if (msg.caller == giver and msg.caller != receiver) {
                result := List.push((receiver, List.foldLeft(amounts, 0 : Float, Float.add)), result);
            };
        };
        result;
    };
};