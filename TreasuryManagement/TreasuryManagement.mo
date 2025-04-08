import Map "mo:base/OrderedMap";
import Nat "mo:base/Nat";
import Principal "mo:base/Principal";
import Text "mo:base/Text";
import Iter "mo:base/Iter";
import Debug "mo:base/Debug";
import Time "mo:base/Time";

persistent actor TreasuryManagement {

    public type UserId = Principal;
    public type BudgetId = Nat;
    public type TransactionId = Nat;
    public type Amount = Nat;
    public type Category = Text;

    public type Transaction = {
        id : TransactionId;
        user : UserId;
        amount : Amount;
        txType : { #Deposit; #Withdrawal };
        category : ?Category;
        timestamp : Time.Time;
    };

    public type Budget = {
        id : BudgetId;
        category : Category;
        allocation : Amount;
        spent : Amount;
        active : Bool;
    };

    public type SpendingLimit = {
        category : Category;
        limit : Amount;
        period : { #daily; #weekly; #monthly };
    };

    // Treasury state
    var balance : Amount = 0;
    var nextTransactionId : Nat = 0;
    var nextBudgetId : Nat = 0;

    transient let transactionMap = Map.Make<TransactionId>(Nat.compare);
    var transactions : Map.Map<TransactionId, Transaction> = transactionMap.empty();

    transient let budgetMap = Map.Make<BudgetId>(Nat.compare);
    var budgets : Map.Map<BudgetId, Budget> = budgetMap.empty();

    transient let spendingLimitMap = Map.Make<Category>(Text.compare);
    var spendingLimits : Map.Map<Category, SpendingLimit> = spendingLimitMap.empty();

    // Deposit funds into the treasury
    public shared ({ caller = user }) func deposit(amount : Amount) : async TransactionId {
        // Validate deposit amount
        if (amount == 0) {
            Debug.trap("Deposit amount must be greater than zero");
        };

        // In a real system, we would integrate with a token contract here

        // Update treasury balance
        balance += amount;

        // Record transaction
        let txId = nextTransactionId;
        nextTransactionId += 1;

        let transaction : Transaction = {
            id = txId;
            user;
            amount;
            txType = #Deposit;
            category = null;
            timestamp = Time.now();
        };

        transactions := transactionMap.put(transactions, txId, transaction);
        txId;
    };

    // Withdraw funds from the treasury
    public shared ({ caller = user }) func withdraw(amount : Amount, category : ?Category) : async TransactionId {
        // Validate withdrawal
        if (amount == 0) {
            Debug.trap("Withdrawal amount must be greater than zero");
        };

        if (amount > balance) {
            Debug.trap("Insufficient treasury balance");
        };

        // Check against budget and spending limits if category provided
        switch (category) {
            case (?cat) {
                // Check if category has budget
                let categoryBudgets = Iter.toArray(
                    Iter.filter(
                        budgetMap.entries(budgets),
                        func((_, b) : (BudgetId, Budget)) : Bool {
                            b.category == cat and b.active;
                        },
                    )
                );

                // If category has a budget, validate against it
                if (categoryBudgets.size() > 0) {
                    let (budgetId, budget) = categoryBudgets[0];
                    if (budget.spent + amount > budget.allocation) {
                        Debug.trap("Withdrawal exceeds budget allocation for category: " # cat);
                    };

                    // Update budget spent amount
                    let updatedBudget = {
                        budget with
                        spent = budget.spent + amount;
                    };

                    budgets := budgetMap.put(budgets, budgetId, updatedBudget);
                };

                switch (spendingLimitMap.get(spendingLimits, cat)) {
                    case (?limit) {
                        // In a real system, we would implement period checks here
                        // For simplicity, we're not calculating total spent in period
                        // Just checking against the limit directly
                        if (amount > limit.limit) {
                            Debug.trap("Withdrawal exceeds spending limit for category: " # cat);
                        };
                    };
                    case (null) {
                        // No spending limit for this category
                    };
                };
            };
            case (null) {
                // No category specified, no budget or limit checks needed
            };
        };

        // Update treasury balance
        balance -= amount;

        // Record transaction
        let txId = nextTransactionId;
        nextTransactionId += 1;

        let transaction : Transaction = {
            id = txId;
            user;
            amount;
            txType = #Withdrawal;
            category;
            timestamp = Time.now();
        };

        transactions := transactionMap.put(transactions, txId, transaction);
        txId;
    };

    // Query treasury balance
    public query func getBalance() : async Amount {
        balance;
    };

    // Query transaction history
    public query func getTransaction(txId : TransactionId) : async Transaction {
        let ?tx = transactionMap.get(transactions, txId) else Debug.trap("Transaction not found");
        tx;
    };

    // Query all transactions
    public query func getAllTransactions() : async [Transaction] {
        Iter.toArray(
            Iter.map(
                transactionMap.entries(transactions),
                func((_, tx) : (TransactionId, Transaction)) : Transaction {
                    tx;
                },
            )
        );
    };

    // Create a budget
    public shared func createBudget(category : Category, allocation : Amount) : async BudgetId {
        let budgetId = nextBudgetId;
        nextBudgetId += 1;

        let budget : Budget = {
            id = budgetId;
            category;
            allocation;
            spent = 0;
            active = true;
        };

        budgets := budgetMap.put(budgets, budgetId, budget);
        budgetId;
    };

    // Update budget allocation
    public shared func updateBudget(budgetId : BudgetId, newAllocation : Amount) : async () {
        let ?budget = budgetMap.get(budgets, budgetId) else Debug.trap("Budget not found");

        let updatedBudget = {
            budget with
            allocation = newAllocation;
        };

        budgets := budgetMap.put(budgets, budgetId, updatedBudget);
    };

    // Query budget details
    public query func getBudget(budgetId : BudgetId) : async Budget {
        let ?budget = budgetMap.get(budgets, budgetId) else Debug.trap("Budget not found");
        budget;
    };

    // Set spending limit
    public shared func setSpendingLimit(
        category : Category,
        limit : Amount,
        period : { #daily; #weekly; #monthly },
    ) : async () {
        let spendingLimit : SpendingLimit = {
            category;
            limit;
            period;
        };

        spendingLimits := spendingLimitMap.put(spendingLimits, category, spendingLimit);
    };

    // Get spending limit
    public query func getSpendingLimit(category : Category) : async ?SpendingLimit {
        spendingLimitMap.get(spendingLimits, category);
    };

    // Get all active budgets
    public query func getActiveBudgets() : async [Budget] {
        Iter.toArray(
            Iter.map(
                Iter.filter(
                    budgetMap.entries(budgets),
                    func((_, b) : (BudgetId, Budget)) : Bool { b.active },
                ),
                func((_, b) : (BudgetId, Budget)) : Budget { b },
            )
        );
    };
};