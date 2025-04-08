import Principal "mo:base/Principal";
import Text "mo:base/Text";
import Nat "mo:base/Nat";
import Iter "mo:base/Iter";
import OrderedMap "mo:base/OrderedMap";

persistent actor DAOLoyaltyProgram {

    type RewardProposal = {
        id : Nat;
        retailer : Principal;
        description : Text;
        votes : Nat;
        approved : Bool;
    };

    type MemberReward = {
        memberId : Principal;
        points : Nat;
    };

    // Initialize OrderedMap operations
    transient let proposalOps = OrderedMap.Operations<Nat>(Nat.compare);
    transient let memberRewardOps = OrderedMap.Operations<Principal>(Principal.compare);

    // Storage for proposals and rewards
    var proposals = proposalOps.empty<RewardProposal>();
    var memberRewards = memberRewardOps.empty<MemberReward>();

    // Propose a new reward
    public func proposeReward(retailer : Principal, description : Text) : async Nat {
        let id = proposalOps.size(proposals);
        let newProposal : RewardProposal = {
            id;
            retailer;
            description;
            votes = 0;
            approved = false;
        };
        proposals := proposalOps.put(proposals, id, newProposal);
        return id;
    };

    // Vote for a proposal
    public func voteForProposal(proposalId : Nat) : async Bool {
        let ?proposal = proposalOps.get(proposals, proposalId) else return false;
        let updatedProposal = { proposal with votes = proposal.votes + 1 };
        proposals := proposalOps.put(proposals, proposalId, updatedProposal);
        return true;
    };

    // Approve a proposal if it meets the vote threshold
    public func approveProposal(proposalId : Nat, threshold : Nat) : async Bool {
        let ?proposal = proposalOps.get(proposals, proposalId) else return false;
        if (proposal.votes >= threshold) {
            let updatedProposal = { proposal with approved = true };
            proposals := proposalOps.put(proposals, proposalId, updatedProposal);
            return true;
        };
        return false;
    };

    // Add reward points to a member's account
    public func addRewardPoints(memberId : Principal, points : Nat) : async Bool {
        let ?reward = memberRewardOps.get(memberRewards, memberId) else {
            let newReward : MemberReward = { memberId; points };
            memberRewards := memberRewardOps.put(memberRewards, memberId, newReward);
            return true;
        };

        let updatedReward = { reward with points = reward.points + points };
        memberRewards := memberRewardOps.put(memberRewards, memberId, updatedReward);
        return true;
    };

    // Redeem points from a member's account
    public func redeemPoints(memberId : Principal, points : Nat) : async Bool {
        let ?reward = memberRewardOps.get(memberRewards, memberId) else return false;
        if (reward.points < points) return false;

        let updatedReward = {
            reward with points = Nat.sub(reward.points, points)
        };
        memberRewards := memberRewardOps.put(memberRewards, memberId, updatedReward);
        return true;
    };

    // Get a proposal by ID
    public query func getProposal(proposalId : Nat) : async ?RewardProposal {
        proposalOps.get(proposals, proposalId);
    };

    // Get all proposals
    public query func getAllProposals() : async [RewardProposal] {
        Iter.toArray(proposalOps.vals(proposals));
    };

    // Get member rewards by ID
    public query func getMemberRewards(memberId : Principal) : async ?MemberReward {
        memberRewardOps.get(memberRewards, memberId);
    };

    // Get all member rewards
    public query func getAllMemberRewards() : async [MemberReward] {
        Iter.toArray(memberRewardOps.vals(memberRewards));
    };
};