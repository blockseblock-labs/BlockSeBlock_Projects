import Nat "mo:base/Nat";
import Time "mo:base/Time";
import Principal "mo:base/Principal";
import Float "mo:base/Float";
import OrderedMap "mo:base/OrderedMap";
import Int "mo:base/Int";

persistent actor StakingContract {
    type Time = Time.Time;
    type Stake = {
        amount : Nat;
        startTime : Time;
        lastClaimTime : Time;
    };

    let rewardRate : Float = 0.000001;
    let penaltyRate : Float = 0.10;
    let minStakeDuration : Time = 5 * 60 * 1000000000; // 5 minutes in nanoseconds
    transient let stakeMap = OrderedMap.Make<Principal>(Principal.compare);
    var stakes : OrderedMap.Map<Principal, Stake> = stakeMap.empty<Stake>();

    public shared ({ caller }) func stake(amount : Nat) : async () {
        let currentTime = Time.now();
        let newStake : Stake = {
            amount = amount;
            startTime = currentTime;
            lastClaimTime = currentTime;
        };
        stakes := stakeMap.put(stakes, caller, newStake);
    };

    public shared ({ caller }) func unstake() : async Nat {
        let ?stake = stakeMap.get(stakes, caller) else return 0;
        let currentTime = Time.now();
        let timeStaked = currentTime - stake.startTime;
        var finalAmount = stake.amount;
        if (timeStaked < minStakeDuration) {
            finalAmount := Int.abs(Float.toInt(Float.fromInt(stake.amount) * (1.0 - penaltyRate)));
        };
        let (_stakes, _old) = stakeMap.remove(stakes, caller);
        stakes := _stakes;
        finalAmount;
    };

    public shared ({ caller }) func claimRewards() : async ?Nat {
        let currentTime = Time.now();
        let ?stake = stakeMap.get(stakes, caller) else return null;
        let timeElapsed = currentTime - stake.lastClaimTime;
        let rewards = Int.abs(Float.toInt(Float.fromInt(stake.amount) * rewardRate * Float.fromInt(timeElapsed)));
        let updatedStake : Stake = {
            amount = stake.amount;
            startTime = stake.startTime;
            lastClaimTime = currentTime;
        };
        stakes := stakeMap.put(stakes, caller, updatedStake);
        ?rewards;
    };
    public shared ({ caller }) func getStakeAmount() : async Nat {
        let ?stakeAmount = stakeMap.get(stakes, caller) else return 0;
        stakeAmount.amount;
    };
};