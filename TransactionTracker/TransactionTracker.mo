import Time "mo:base/Time";
import OrderedMap "mo:base/OrderedMap";
import Principal "mo:base/Principal";
import Nat "mo:base/Nat";
import Iter "mo:base/Iter";
import Order "mo:base/Order";
import Array "mo:base/Array";

persistent actor TransactionManager {

    // Transavtion Struct
    type Transaction = {
        id : Nat;
        sender : Principal;
        receiver : Principal;
        timestamp : Time.Time;
        amount : Nat;
    };

    // Transaction Compare
    private transient let transactionOps = OrderedMap.Make<Nat>(
        func(a : Nat, b : Nat) : Order.Order {
            Nat.compare(a, b);
        }
    );

    // User Transaction Compare
    private transient let userTransactionOps = OrderedMap.Make<Principal>(
        func(a : Principal, b : Principal) : Order.Order {
            Principal.compare(a, b);
        }
    );

    // State Variables which hold the transactions and user transactions
    private var transactions = transactionOps.empty<Transaction>();
    private var userTransactions = userTransactionOps.empty<[Transaction]>();
    private var transactionCounter : Nat = 0;

    // Record Transaction
    public shared ({ caller }) func recordTransaction(receiver : Principal, amount : Nat) : async Text {
        let sender = caller;
        if (amount <= 0) {
            return "Error: Amount must be greater than zero.";
        };

        let transaction : Transaction = {
            id = transactionCounter;
            sender = sender;
            receiver = receiver;
            amount = amount;
            timestamp = Time.now();
        };

        transactions := transactionOps.put(transactions, transactionCounter, transaction);

        let senderHistory : [Transaction] = switch (userTransactionOps.get(userTransactions, sender)) {
            case (?list) list;
            case null [];
        };
        userTransactions := userTransactionOps.put(
            userTransactions,
            sender,
            Array.append(
                senderHistory,
                [transaction],
            ),
        );

        let receiverHistory = switch (userTransactionOps.get(userTransactions, receiver)) {
            case (?list) list;
            case null [];
        };
        userTransactions := userTransactionOps.put(userTransactions, receiver, Array.append(receiverHistory, [transaction]));
        transactionCounter += 1;

        return "Transaction recorded successfully!";
    };

    // Get Transaction History (For Sender and Receiver, avoiding duplicates)
    public query func getTransactionHistory(user : Principal) : async [Transaction] {
        // Get sender transactions
        let senderHistory = switch (userTransactionOps.get(userTransactions, user)) {
            case (?history) history;
            case null [];
        };

        // Get receiver transactions
        let receiverHistory = switch (userTransactionOps.get(userTransactions, user)) {
            case (?history) history;
            case null [];
        };

        // Combine sender and receiver histories
        let combinedHistory = Array.append(senderHistory, receiverHistory);

        // Use an OrderedMap to remove duplicates based on transaction id
        var uniqueTransactions = transactionOps.empty<Transaction>();

        // Add transactions to the map to ensure uniqueness by id
        for (transaction in Iter.fromArray(combinedHistory)) {
            uniqueTransactions := transactionOps.put(uniqueTransactions, transaction.id, transaction);
        };

        // Convert the OrderedMap values to an array and return
        return Iter.toArray(transactionOps.vals(uniqueTransactions));
    };

    // Get All Transactions
    public query func getAllTransactions() : async [Transaction] {
        Iter.toArray(transactionOps.vals(transactions));
    };

    // Get Total Amount By User
    public query func getTotalAmountByUser(user : Principal) : async Nat {
        let history = switch (userTransactionOps.get(userTransactions, user)) {
            case (?transactions) transactions;
            case null [];
        };

        let totalAmount = Array.foldLeft<Transaction, Nat>(
            history,
            0,
            func(amount, transaction) {
                amount + transaction.amount;
            },
        );

        return totalAmount;
    };
};