import Map "mo:base/OrderedMap";
import Principal "mo:base/Principal";
import Nat "mo:base/Nat";
import Time "mo:base/Time";
import Iter "mo:base/Iter";
import Float "mo:base/Float";
import Array "mo:base/Array";
import Int "mo:base/Int";
import Debug "mo:base/Debug";

persistent actor DebtTokenization {

    public type LoanId = Nat;
    public type TokenId = Nat;
    public type FractionId = Nat;

    public type Loan = {
        id : LoanId;
        borrower : Principal;
        lender : Principal;
        amount : Nat;
        interestRate : Float;
        termDays : Nat;
        issuanceTime : Time.Time;
        maturityTime : Time.Time;
        status : { #active; #repaid; #defaulted; #tokenized };
        fractionalized : Bool;
        yieldPerDay : Float;
    };

    public type DebtToken = {
        id : TokenId;
        loanId : LoanId;
        owner : Principal;
        value : Nat;
        issuanceTime : Time.Time;
        lastYieldClaimTime : Time.Time;
        accruedYield : Nat;
    };

    public type DebtFraction = {
        id : FractionId;
        loanId : LoanId;
        tokenId : TokenId;
        owner : Principal;
        fraction : Float; // Percentage of the original token
        value : Nat;
        issuanceTime : Time.Time;
        lastYieldClaimTime : Time.Time;
        accruedYield : Nat;
    };

    public type MarketListing = {
        tokenId : ?TokenId;
        fractionId : ?FractionId;
        seller : Principal;
        price : Nat;
        listingTime : Time.Time;
    };

    public type RepaymentSchedule = {
        loanId : LoanId;
        totalAmount : Nat;
        amountPaid : Nat;
        nextPaymentDue : Time.Time;
        paymentIntervalDays : Nat;
        paymentsRemaining : Nat;
    };

    var nextLoanId : LoanId = 0;
    var nextTokenId : TokenId = 0;
    var nextFractionId : FractionId = 0;

    transient let loanMap = Map.Make<LoanId>(Nat.compare);
    var loans : Map.Map<LoanId, Loan> = loanMap.empty();

    transient let tokenMap = Map.Make<TokenId>(Nat.compare);
    var tokens : Map.Map<TokenId, DebtToken> = tokenMap.empty();

    transient let fractionMap = Map.Make<FractionId>(Nat.compare);
    var fractions : Map.Map<FractionId, DebtFraction> = fractionMap.empty();

    transient let listingMap = Map.Make<Nat>(Nat.compare);
    var listings : Map.Map<Nat, MarketListing> = listingMap.empty();

    transient let repaymentMap = Map.Make<LoanId>(Nat.compare);
    var repayments : Map.Map<LoanId, RepaymentSchedule> = repaymentMap.empty();

    let BASE_YIELD_MULTIPLIER : Float = 0.95; // 95% of interest rate becomes yield
    // let PLATFORM_FEE : Float = 0.005; // 0.5% fee on transactions
    let SECONDS_PER_DAY : Nat = 86400;

    func floatToNat(x : Float) : Nat {
        if (x < 0) {
            return 0;
        };
        let floored = Float.floor(x);
        let asInt = Float.toInt(floored);
        if (asInt < 0) {
            return 0;
        };
        return Int.abs(asInt);
    };

    // Create a new loan
    public shared ({ caller }) func createLoan(
        borrower : Principal,
        amount : Nat,
        interestRate : Float,
        termDays : Nat,
    ) : async LoanId {
        let loanId = nextLoanId;
        nextLoanId += 1;

        let now = Time.now();
        let maturityTimeNanos = now + Int.abs(termDays * SECONDS_PER_DAY * 1_000_000_000);

        let newLoan : Loan = {
            id = loanId;
            borrower = borrower;
            lender = caller;
            amount = amount;
            interestRate = interestRate;
            termDays = termDays;
            issuanceTime = now;
            maturityTime = maturityTimeNanos;
            status = #active;
            fractionalized = false;
            yieldPerDay = (interestRate * BASE_YIELD_MULTIPLIER) / 365.0;
        };

        loans := loanMap.put(loans, loanId, newLoan);

        let paymentInterval = termDays / 12; // Monthly payments by default
        let paymentIntervalAdjusted = if (paymentInterval < 1) { 1 } else {
            paymentInterval;
        };

        let interestAmount = floatToNat(Float.fromInt(amount) * interestRate);
        let totalAmount = amount + interestAmount;

        let schedule : RepaymentSchedule = {
            loanId = loanId;
            totalAmount = totalAmount;
            amountPaid = 0;
            nextPaymentDue = now + Int.abs(paymentIntervalAdjusted * SECONDS_PER_DAY * 1_000_000_000);
            paymentIntervalDays = paymentIntervalAdjusted;
            paymentsRemaining = 12; // 12 payments by default
        };

        repayments := repaymentMap.put(repayments, loanId, schedule);
        return loanId;
    };

    // Tokenize a loan - convert it to an NFT
    public shared ({ caller }) func tokenizeLoan(loanId : LoanId) : async TokenId {
        switch (loanMap.get(loans, loanId)) {
            case (null) { Debug.trap("Loan not found") };
            case (?loan) {
                if (loan.lender != caller) {
                    Debug.trap("Only the lender can tokenize a loan");
                };

                if (loan.status != #active) {
                    Debug.trap("Loan is not in active status");
                };

                let tokenId = nextTokenId;
                nextTokenId += 1;

                let now = Time.now();
                let newToken : DebtToken = {
                    id = tokenId;
                    loanId = loanId;
                    owner = caller;
                    value = loan.amount;
                    issuanceTime = now;
                    lastYieldClaimTime = now;
                    accruedYield = 0;
                };

                tokens := tokenMap.put(tokens, tokenId, newToken);

                let updatedLoan : Loan = {
                    loan with
                    status = #tokenized;
                };

                loans := loanMap.put(loans, loanId, updatedLoan);
                return tokenId;
            };
        };
    };

    // Fractionalize a debt token
    public shared ({ caller }) func fractionalizeToken(
        tokenId : TokenId,
        fractionCount : Nat,
    ) : async [FractionId] {
        if (fractionCount < 2 or fractionCount > 100) {
            Debug.trap("Fractions must be between 2 and 100");
        };

        switch (tokenMap.get(tokens, tokenId)) {
            case (null) { Debug.trap("Token not found") };
            case (?token) {
                if (token.owner != caller) {
                    Debug.trap("Only the token owner can fractionalize");
                };

                let fractionValue = token.value / fractionCount;
                let fractionPercent : Float = 1.0 / Float.fromInt(fractionCount);
                let now = Time.now();

                var fractionIds : [FractionId] = [];

                for (i in Iter.range(0, fractionCount - 1)) {
                    let fractionId = nextFractionId;
                    nextFractionId += 1;

                    let newFraction : DebtFraction = {
                        id = fractionId;
                        loanId = token.loanId;
                        tokenId = tokenId;
                        owner = caller;
                        fraction = fractionPercent;
                        value = fractionValue;
                        issuanceTime = now;
                        lastYieldClaimTime = now;
                        accruedYield = 0;
                    };

                    fractions := fractionMap.put(fractions, fractionId, newFraction);
                    fractionIds := Array.append(fractionIds, [fractionId]);
                };

                switch (loanMap.get(loans, token.loanId)) {
                    case (null) { Debug.trap("Loan not found") };
                    case (?loan) {
                        let updatedLoan : Loan = {
                            loan with
                            fractionalized = true;
                        };

                        loans := loanMap.put(loans, loan.id, updatedLoan);
                    };
                };

                return fractionIds;
            };
        };
    };

    // List a token or fraction for sale
    public shared ({ caller }) func listForSale(
        itemId : Nat,
        isToken : Bool,
        price : Nat,
    ) : async () {
        if (isToken) {
            switch (tokenMap.get(tokens, itemId)) {
                case (null) { Debug.trap("Token not found") };
                case (?token) {
                    if (token.owner != caller) {
                        Debug.trap("Only the owner can list a token");
                    };

                    let listing : MarketListing = {
                        tokenId = ?itemId;
                        fractionId = null;
                        seller = caller;
                        price = price;
                        listingTime = Time.now();
                    };

                    listings := listingMap.put(listings, itemId, listing);
                };
            };
        } else {
            switch (fractionMap.get(fractions, itemId)) {
                case (null) { Debug.trap("Fraction not found") };
                case (?fraction) {
                    if (fraction.owner != caller) {
                        Debug.trap("Only the owner can list a fraction");
                    };

                    let listing : MarketListing = {
                        tokenId = null;
                        fractionId = ?itemId;
                        seller = caller;
                        price = price;
                        listingTime = Time.now();
                    };

                    listings := listingMap.put(listings, itemId + 1_000_000, listing);
                };
            };
        };
    };

    // Buy a listed token or fraction
    public shared ({ caller }) func buyListed(listingId : Nat) : async () {
        let _actualId : Nat = if (listingId >= 1_000_000) {
            listingId - 1_000_000;
        } else {
            listingId;
        };

        switch (listingMap.get(listings, listingId)) {
            case (null) { Debug.trap("Listing not found") };
            case (?listing) {
                if (caller == listing.seller) {
                    Debug.trap("Cannot buy your own listing");
                };

                switch (listing.tokenId) {
                    case (null) { /* Not a token listing */ };
                    case (?tokenId) {
                        switch (tokenMap.get(tokens, tokenId)) {
                            case (null) { Debug.trap("Token not found") };
                            case (?token) {
                                // In a real implementation, handle payment here

                                let updatedToken : DebtToken = {
                                    token with
                                    owner = caller;
                                };

                                tokens := tokenMap.put(tokens, tokenId, updatedToken);
                            };
                        };
                    };
                };

                switch (listing.fractionId) {
                    case (null) { /* Not a fraction listing */ };
                    case (?fractionId) {
                        switch (fractionMap.get(fractions, fractionId)) {
                            case (null) { Debug.trap("Fraction not found") };
                            case (?fraction) {
                                // In a real implementation, handle payment here

                                let updatedFraction : DebtFraction = {
                                    fraction with
                                    owner = caller;
                                };

                                fractions := fractionMap.put(fractions, fractionId, updatedFraction);
                            };
                        };
                    };
                };

                listings := listingMap.remove(listings, listingId).0;
            };
        };
    };

    // Make a loan repayment
    public shared ({ caller }) func makeRepayment(
        loanId : LoanId,
        amount : Nat,
    ) : async () {
        switch (loanMap.get(loans, loanId)) {
            case (null) { Debug.trap("Loan not found") };
            case (?loan) {
                if (loan.borrower != caller) {
                    Debug.trap("Only the borrower can make repayments");
                };

                if (loan.status != #active and loan.status != #tokenized) {
                    Debug.trap("Loan is not active or tokenized");
                };

                switch (repaymentMap.get(repayments, loanId)) {
                    case (null) { Debug.trap("Repayment schedule not found") };
                    case (?schedule) {
                        let newAmountPaid = schedule.amountPaid + amount;
                        let now = Time.now();

                        let updatedSchedule : RepaymentSchedule = {
                            schedule with
                            amountPaid = newAmountPaid;
                            nextPaymentDue = now + Int.abs(schedule.paymentIntervalDays * SECONDS_PER_DAY * 1_000_000_000);
                            paymentsRemaining : Nat = if (schedule.paymentsRemaining > 0) {
                                schedule.paymentsRemaining - 1;
                            } else {
                                0;
                            };
                        };

                        repayments := repaymentMap.put(repayments, loanId, updatedSchedule);

                        if (loan.status == #tokenized) {
                            await distributeYield(loanId, amount);
                        };

                        if (newAmountPaid >= schedule.totalAmount or updatedSchedule.paymentsRemaining == 0) {
                            let repaidLoan : Loan = {
                                loan with
                                status = #repaid;
                            };

                            loans := loanMap.put(loans, loanId, repaidLoan);
                        };
                    };
                };
            };
        };
    };

    // Distribute yield to token or fraction holders
    public func distributeYield(
        loanId : LoanId,
        amount : Nat,
    ) : async () {
        let tokensForLoan = Iter.toArray(
            Iter.filter(
                tokenMap.entries(tokens),
                func((_id : TokenId, token : DebtToken)) : Bool {
                    token.loanId == loanId;
                },
            )
        );

        // If there are no fractions, distribute to token holder
        if (tokensForLoan.size() > 0) {
            let fractionsForLoan = Iter.toArray(
                Iter.filter(
                    fractionMap.entries(fractions),
                    func((_id : FractionId, fraction : DebtFraction)) : Bool {
                        fraction.loanId == loanId;
                    },
                )
            );

            if (fractionsForLoan.size() == 0) {
                // Distribute to token holders
                for ((tokenId, token) in tokensForLoan.vals()) {
                    let updatedToken : DebtToken = {
                        token with
                        accruedYield = token.accruedYield + amount;
                        lastYieldClaimTime = Time.now();
                    };

                    tokens := tokenMap.put(tokens, tokenId, updatedToken);
                };
            } else {
                for ((fractionId, fraction) in fractionsForLoan.vals()) {
                    let fractionAmount = floatToNat(Float.fromInt(amount) * fraction.fraction);

                    let updatedFraction : DebtFraction = {
                        fraction with
                        accruedYield = fraction.accruedYield + fractionAmount;
                        lastYieldClaimTime = Time.now();
                    };

                    fractions := fractionMap.put(fractions, fractionId, updatedFraction);
                };
            };
        };
    };

    // Claim accrued yield
    public shared ({ caller }) func claimYield(
        isToken : Bool,
        id : Nat,
    ) : async Nat {
        if (isToken) {
            switch (tokenMap.get(tokens, id)) {
                case (null) { Debug.trap("Token not found") };
                case (?token) {
                    if (token.owner != caller) {
                        Debug.trap("Only the owner can claim yield");
                    };

                    let yieldAmount = token.accruedYield;

                    if (yieldAmount == 0) {
                        Debug.trap("No yield to claim");
                    };

                    // In a real implementation, transfer yield to caller

                    let updatedToken : DebtToken = {
                        token with
                        accruedYield = 0;
                        lastYieldClaimTime = Time.now();
                    };

                    tokens := tokenMap.put(tokens, id, updatedToken);

                    return yieldAmount;
                };
            };
        } else {
            switch (fractionMap.get(fractions, id)) {
                case (null) { Debug.trap("Fraction not found") };
                case (?fraction) {
                    if (fraction.owner != caller) {
                        Debug.trap("Only the owner can claim yield");
                    };

                    let yieldAmount = fraction.accruedYield;

                    if (yieldAmount == 0) {
                        Debug.trap("No yield to claim");
                    };

                    // In a real implementation, transfer yield to caller

                    let updatedFraction : DebtFraction = {
                        fraction with
                        accruedYield = 0;
                        lastYieldClaimTime = Time.now();
                    };

                    fractions := fractionMap.put(fractions, id, updatedFraction);

                    return yieldAmount;
                };
            };
        };
    };

    // Get loan details
    public query func getLoan(loanId : LoanId) : async Loan {
        switch (loanMap.get(loans, loanId)) {
            case (null) { Debug.trap("Loan not found") };
            case (?loan) { return loan };
        };
    };

    // Get token details
    public query func getToken(tokenId : TokenId) : async DebtToken {
        switch (tokenMap.get(tokens, tokenId)) {
            case (null) { Debug.trap("Token not found") };
            case (?token) { return token };
        };
    };

    // Get fraction details
    public query func getFraction(fractionId : FractionId) : async DebtFraction {
        switch (fractionMap.get(fractions, fractionId)) {
            case (null) { Debug.trap("Fraction not found") };
            case (?fraction) { return fraction };
        };
    };

    // Get all active listings
    public query func getActiveListings() : async [MarketListing] {
        Iter.toArray(listingMap.vals(listings));
    };

    // Get repayment schedule
    public query func getRepaymentSchedule(loanId : LoanId) : async RepaymentSchedule {
        switch (repaymentMap.get(repayments, loanId)) {
            case (null) { Debug.trap("Repayment schedule not found") };
            case (?schedule) { return schedule };
        };
    };

    // Process automatic repayments (triggered by a timer or external call)
    public func processAutomaticRepayments() : async () {
        let now = Time.now();

        for ((loanId, schedule) in repaymentMap.entries(repayments)) {
            if (schedule.nextPaymentDue <= now and schedule.paymentsRemaining > 0) {
                // In a real implementation, attempt to collect payment from borrower

                // For now, just update the schedule
                let updatedSchedule : RepaymentSchedule = {
                    schedule with
                    nextPaymentDue = now + Int.abs(schedule.paymentIntervalDays * SECONDS_PER_DAY * 1_000_000_000);
                };

                repayments := repaymentMap.put(repayments, loanId, updatedSchedule);
            };
        };
    };
};