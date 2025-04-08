import Nat "mo:base/Nat";
import Nat64 "mo:base/Nat64";
import Float "mo:base/Float";
import Principal "mo:base/Principal";
import OrderedMap "mo:base/OrderedMap";
import Time "mo:base/Time";
import Int "mo:base/Int";

persistent actor LiquidityMining {
    type User = {
        stakeAmount : Nat;
        stakeTimestamp : Nat64;
        rewardDebt : Nat;
    };
    type RewardConfig = {
        initialRewardRate : Nat;
        decayFactor : Float;
        minStakeTime : Nat64;
    };
    type Withdraw = {
        #successful;
        #noStakeFound;
        #insufficientStakeDuration;
    };

    transient let orderedMap = OrderedMap.Make<Principal>(Principal.compare);
    var users : OrderedMap.Map<Principal, User> = orderedMap.empty<User>();
    var totalStaked : Nat = 0;
    let rewardConfig : RewardConfig = {
        initialRewardRate = 100;
        decayFactor = 0.95;
        minStakeTime = 60 * 60 * 24 * 7;
    };
    var contractStartTime : Nat64 = Nat64.fromNat(Int.abs(Time.now()));
    func getCurrentRewardRate() : Float {
        let elapsedTime = Float.fromInt(Nat64.toNat(Nat64.fromNat(Int.abs(Time.now())) - contractStartTime));
        Float.fromInt(rewardConfig.initialRewardRate) * Float.pow(rewardConfig.decayFactor, elapsedTime / 1_000_000_000.0);
    };
    public shared ({ caller }) func stake(amount : Nat) : async () {
        let currentTime = Nat64.fromNat(Int.abs(Time.now()));
        let user = switch (orderedMap.get(users, caller)) {
            case (?u) u;
            case null {
                {
                    stakeAmount = 0;
                    stakeTimestamp = currentTime;
                    rewardDebt = 0;
                };
            };
        };
        users := orderedMap.put(
            users,
            caller,
            {
                stakeAmount = user.stakeAmount + amount;
                stakeTimestamp = currentTime;
                rewardDebt = user.rewardDebt;
            },
        );
        totalStaked += amount;
    };

    public shared ({ caller }) func withdraw() : async Withdraw {
        let currentTime = Nat64.fromNat(Int.abs(Time.now()));
        let ?user = orderedMap.get(users, caller) else return #noStakeFound;
        if (currentTime - user.stakeTimestamp < rewardConfig.minStakeTime) {
            return #insufficientStakeDuration;
        };
        let amount = user.stakeAmount;
        let (usersNew, _) = orderedMap.remove(users, caller);
        users := usersNew;
        totalStaked -= amount;
        #successful;
    };
    public shared ({ caller }) func claimRewards() : async ?Nat {
        let currentTime = Nat64.fromNat(Int.abs(Time.now()));
        let ?user = orderedMap.get(users, caller) else return null;
        let rewardRate = getCurrentRewardRate();
        let stakingDuration = Float.fromInt(Nat64.toNat(currentTime - user.stakeTimestamp));
        let rewards = Nat64.fromNat(Int.abs(Float.toInt(Float.fromInt(user.stakeAmount) * rewardRate * stakingDuration / 1_000_000_000.0)));
        users := orderedMap.put(users, caller, { stakeAmount = user.stakeAmount; stakeTimestamp = currentTime; rewardDebt = 0 });
        ?Nat64.toNat(rewards);
    };
    public shared ({ caller }) func getStake() : async ?Nat {
        let ?user = orderedMap.get(users, caller) else return null;
        ?user.stakeAmount;
    };
};