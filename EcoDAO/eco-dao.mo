import Text "mo:base/Text";
import OrderedMap "mo:base/OrderedMap";
import Iter "mo:base/Iter";
import Nat "mo:base/Nat";

persistent actor EcoTourismDAO {

    type Proposal = {
        id : Nat;
        operator : Text;
        description : Text;
        carbonOffset : Nat;
        voteCount : Nat;
        approved : Bool;
    };

    type Review = {
        reviewer : Text;
        tripId : Nat;
        rating : Nat;
        comment : Text;
    };

    // Initialize OrderedMap operations
    transient let ProposalMapOps = OrderedMap.Operations<Nat>(Nat.compare);
    transient let ReviewMapOps = OrderedMap.Operations<Nat>(Nat.compare);
    transient let HolderMapOps = OrderedMap.Operations<Text>(Text.compare);

    // Storage for proposals, reviews, and token holders
    var proposals = ProposalMapOps.empty<Proposal>();
    var reviews = ReviewMapOps.empty<Review>();
    var tokenHolders = HolderMapOps.empty<Bool>();
    var proposalCounter : Nat = 0;

    // Add a token holder
    public func addTokenHolder(holder : Text) : async () {
        tokenHolders := HolderMapOps.put(tokenHolders, holder, true);
    };

    // Submit a new proposal
    public func submitProposal(operator : Text, description : Text, carbonOffset : Nat) : async Nat {
        let newProposal : Proposal = {
            id = proposalCounter;
            operator;
            description;
            carbonOffset;
            voteCount = 0;
            approved = false;
        };
        proposals := ProposalMapOps.put(proposals, proposalCounter, newProposal);
        proposalCounter += 1;
        return newProposal.id;
    };

    // Vote for a proposal
    public func voteProposal(holder : Text, proposalId : Nat) : async Bool {
        if (not isTokenHolder(holder)) {
            return false;
        };

        let ?proposal = ProposalMapOps.get(proposals, proposalId) else return false;
        let updatedProposal = {
            proposal with voteCount = proposal.voteCount + 1
        };
        proposals := ProposalMapOps.put(proposals, proposalId, updatedProposal);
        return true;
    };

    // Finalize a proposal if it meets the required votes
    public func finalizeProposal(proposalId : Nat) : async Bool {
        let ?proposal = ProposalMapOps.get(proposals, proposalId) else return false;
        if (proposal.voteCount >= 3) {
            let updatedProposal = { proposal with approved = true };
            proposals := ProposalMapOps.put(proposals, proposalId, updatedProposal);
            return true;
        };
        return false;
    };

    // Submit a review for a trip
    public func submitReview(reviewer : Text, tripId : Nat, rating : Nat, comment : Text) : async Bool {
        if (rating < 1 or rating > 5) {
            return false;
        };

        let review : Review = {
            reviewer;
            tripId;
            rating;
            comment;
        };

        reviews := ReviewMapOps.put(reviews, tripId, review);
        return true;
    };

    // Check if a holder has tokens
    func isTokenHolder(holder : Text) : Bool {
        let ?_ = HolderMapOps.get(tokenHolders, holder) else return false;
        return true;
    };

    // Get all proposals
    public query func getAllProposals() : async [Proposal] {
        return Iter.toArray(ProposalMapOps.vals(proposals));
    };

    // Get all reviews
    public query func getAllReviews() : async [Review] {
        return Iter.toArray(ReviewMapOps.vals(reviews));
    };
};