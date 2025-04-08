import Principal "mo:base/Principal";
import Nat "mo:base/Nat";
import Text "mo:base/Text";
import Iter "mo:base/Iter";
import Result "mo:base/Result";
import OrderedMap "mo:base/OrderedMap";

persistent actor {
    type Result<T, E> = Result.Result<T, E>;
    type BidId = Nat;
    type AuctionState = {
        #notConfigured;
        #configured : AuctionConfiguration;
        #open : AuctionConfiguration;
        #closed : AuctionConfiguration;
    };
    type AuctionConfiguration = {
        minPriceDecimals : Nat; // e.g., 100 for $1.00
        maxPriceDecimals : Nat; // e.g., 200 for $2.00
        sellTarget : Nat; // e.g., 1_000_000 tokens
    };
    type ClearingPriceDecimals = ?Nat;

    type SaleMetrics = {
        totalFunding : Nat;
        totalTokens : Nat;
        clearingPriceDecimals : Nat;
    };

    type UserBid = {
        bidder : Principal;
        maxPriceDecimals : Nat; // User's max price bid, e.g., 120 for $1.20
        investmentUsdDecimals : Nat; // Desired investment amount, e.g., 10000 for $100.00
    };

    type BidMap = OrderedMap.Map<BidId, UserBid>;
    private transient let bidMap = OrderedMap.Make<BidId>(Nat.compare);

    /// Default admin set to anonymous to allow first setAdmin.
    private var admin : Principal = Principal.fromText("2vxsx-fae");
    private var auctionState : AuctionState = #notConfigured;
    private var bids : BidMap = bidMap.empty();
    private var nextBidId : BidId = 0;
    private var clearingPriceDecimals : ClearingPriceDecimals = null;

    private func isAdmin(caller : Principal) : Bool {
        caller == admin;
    };

    /// Set the admin principal.
    /// The initial (anonymous) admin can be set by anyone.
    /// Only the current admin can set subsequent admins.
    public shared ({ caller }) func setAdmin(newAdmin : Principal) : async Bool {
        if (Principal.isAnonymous(newAdmin)) {
            false;
        } else if (Principal.isAnonymous(admin) or caller == admin) {
            admin := newAdmin;
            true;
        } else {
            false;
        };
    };

    /// Get the admin principal.
    public query func getAdmin() : async Principal {
        admin;
    };

    /// Configure a new auction round (admin only).
    public shared ({ caller }) func configureAuction(minPriceDecimals : Nat, maxPriceDecimals : Nat, sellTarget : Nat) : async Result<(), Text> {
        if (not isAdmin(caller)) {
            return #err("Only admin can configure the auction round.");
        };
        if (minPriceDecimals == 0 or maxPriceDecimals == 0 or sellTarget == 0) {
            return #err("Invalid parameters: all must be > 0.");
        };
        if (minPriceDecimals > maxPriceDecimals) {
            return #err("minPrice must not exceed maxPrice.");
        };

        auctionState := #configured({
            minPriceDecimals;
            maxPriceDecimals;
            sellTarget;
        });
        #ok;
    };

    /// Open the auction round for order submissions (admin only).
    public shared ({ caller }) func openSubmission() : async Result<(), Text> {
        if (not isAdmin(caller)) {
            return #err("Only admin can open the submission window.");
        };
        switch (auctionState) {
            case (#configured(auctionConfiguration)) {
                auctionState := #open(auctionConfiguration);
                #ok;
            };
            case (#open(_)) return #err("Bids are already open.");
            case (#closed(_)) return #err("Auction is already closed.");
            case (#notConfigured) return #err("Auction not configured yet.");
        };
    };

    /// Submit a bid.  Only allowed when the auction is open.
    public shared ({ caller }) func submitBid(maxPriceDecimals : Nat, investmentUsdDecimals : Nat) : async Result<BidId, Text> {
        // Check if the auction is open
        switch (auctionState) {
            case (#open(auctionConfiguration)) {
                if (maxPriceDecimals < auctionConfiguration.minPriceDecimals or maxPriceDecimals > auctionConfiguration.maxPriceDecimals) {
                    return #err("Bid price must be within the configured range.");
                };
                if (investmentUsdDecimals == 0) {
                    return #err("Cannot submit a 0-quantity bid.");
                };
                // investment amount must exceed the min price
                if (investmentUsdDecimals < maxPriceDecimals) {
                    return #err("Investment amount must be greater or equal than bid price.");
                };

                let bidId = nextBidId;
                nextBidId += 1;

                let newBid : UserBid = {
                    bidder = caller;
                    maxPriceDecimals = maxPriceDecimals;
                    investmentUsdDecimals = investmentUsdDecimals;
                };

                bids := bidMap.put(bids, bidId, newBid);

                #ok bidId;
            };
            case (#configured(_)) return #err("Auction is not yet open for bids.");
            case (#closed(_)) return #err("Auction has already closed.");
            case (#notConfigured) return #err("No auction is configured.");
        };
    };

    /// Query your bids (by caller).
    /// This function returns a list of all bids placed by the caller.
    public query ({ caller }) func getMyBids() : async [(BidId, UserBid)] {
        Iter.toArray(
            Iter.filter(
                bidMap.entries(bids),
                func((_ : BidId, b : UserBid)) : Bool { b.bidder == caller },
            )
        );
    };

    /// Close the order submission window. This triggers the pricing engine
    /// to compute the clearing price and finalize allocations.
    /// (admin only)
    public shared ({ caller }) func closeSubmission() : async Result<Nat, Text> {
        if (not isAdmin(caller)) {
            return #err("Only admin can close the submission window.");
        };
        switch (auctionState) {
            case (#open(auctionConfiguration)) {
                auctionState := #closed(auctionConfiguration);

                // Trigger the pricing engine
                let clearingPriceOpt = runPricingEngine(
                    auctionConfiguration.minPriceDecimals,
                    auctionConfiguration.maxPriceDecimals,
                    auctionConfiguration.sellTarget,
                );

                switch (clearingPriceOpt) {
                    case (null) {
                        // Could not find a price that sells the target tokens
                        // In many real scenarios you'd set clearingPrice = minPrice or something else
                        clearingPriceDecimals := null;
                        return #err("No valid clearing price found (perhaps insufficient demand).");
                    };
                    case (?price) {
                        clearingPriceDecimals := ?price;
                        // Optionally, you could also trigger distribution or record final allocations.
                        #ok price;
                    };
                };
            };
            case (#configured(_)) return #err("Submission window hasn't been opened yet.");
            case (#closed(_)) return #err("Auction is already closed.");
            case (#notConfigured) return #err("No auction is configured.");
        };
    };

    /// The main logic for determining the clearing price.
    /// We sweep from minPrice to maxPrice in steps of 1 (assuming integer-based prices).
    private func runPricingEngine(minP : Nat, maxP : Nat, sellTarget : Nat) : ?Nat {
        var bestPrice : ?Nat = null;

        // For each candidate price p in [minP .. maxP], do:
        // 1) Find all bids where maxPrice >= p
        // 2) Sum the total funding F_p
        // 3) Determine total token demand D_p = F_p / p
        // 4) Keep track of the highest p for which D_p >= sellTarget
        var p : Nat = minP;

        // We will store the best found so far
        while (p <= maxP) {
            let filteredBids = Iter.filter(
                bidMap.entries(bids),
                func((_ : BidId, userBid : UserBid)) : Bool {
                    userBid.maxPriceDecimals >= p;
                },
            );
            var totalFunding : Nat = 0;
            for ((_, b) in filteredBids) {
                totalFunding += b.investmentUsdDecimals;
            };

            // D_p = totalFunding / p.
            // Note: minPrice which is the lower bound of p, is bigger than zero. Hence we cannot divide by zero.
            let D_p = totalFunding / p;
            if (D_p >= sellTarget) {
                // This candidate price can sell the entire target at this rate
                // We want the maximum p satisfying D_p >= sellTarget
                bestPrice := ?p;
            };

            p += 1;
        };
        bestPrice;
    };

    /// Query the current clearing price (if any).
    public query func getClearingPrice() : async ?Nat {
        clearingPriceDecimals;
    };

    /// Query the current auction state: `notConfigured`, `configured`, `openForBids`, or `closed`.
    public query func getAuctionState() : async AuctionState {
        auctionState;
    };
};