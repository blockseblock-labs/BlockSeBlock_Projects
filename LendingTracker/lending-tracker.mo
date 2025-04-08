import Trie "mo:base/Trie";
import Text "mo:base/Text";
import Nat "mo:base/Nat";
import Option "mo:base/Option";
import Array "mo:base/Array";

persistent actor LendingTracker {

    type Loan = {
        borrower : Text;
        lender : Text;
        amount : Nat;
        settled : Bool;
    };

    private func key(user : Text) : Trie.Key<Text> {
        { key = user; hash = Text.hash(user) };
    };

    private var loans : Trie.Trie<Text, [Loan]> = Trie.empty();

    public func recordLoan(borrower : Text, lender : Text, amount : Nat) : async Bool {
        if (borrower == lender) { return false }; // Prevent self-loaning

        let newLoan : Loan = {
            borrower;
            lender;
            amount;
            settled = false;
        };

        let existingLoans = Option.get(Trie.get(loans, key(borrower), Text.equal), []);
        loans := Trie.put(loans, key(borrower), Text.equal, Array.append(existingLoans, [newLoan])).0;
        return true;
    };

    public func getTransactionHistory(user : Text) : async [Loan] {
        return Option.get(Trie.get(loans, key(user), Text.equal), []);
    };

    public func markLoanSettled(borrower : Text, index : Nat) : async Bool {
        switch (Trie.get(loans, key(borrower), Text.equal)) {
            case null { return false };
            case (?loanList) {
                if (index >= loanList.size()) {
                    return false;
                } else {
                    let updatedLoanList = Array.tabulate<Loan>(
                        loanList.size(),
                        func(i) {
                            if (i == index) {
                                {
                                    borrower = loanList[i].borrower;
                                    lender = loanList[i].lender;
                                    amount = loanList[i].amount;
                                    settled = true;
                                };
                            } else {
                                loanList[i];
                            };
                        },
                    );
                    loans := Trie.put(loans, key(borrower), Text.equal, updatedLoanList).0;
                    return true;
                };
            };
        };
    };

    public func getBalance(user : Text) : async Nat {
        let userLoans = Option.get(Trie.get(loans, key(user), Text.equal), []);
        var balance : Nat = 0;
        for (loan in userLoans.vals()) {
            if (loan.borrower == user and not loan.settled) {
                balance += loan.amount;
            };
        };
        return balance;
    };
};