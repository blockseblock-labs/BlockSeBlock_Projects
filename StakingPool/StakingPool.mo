import Map "mo:base/OrderedMap";
import Principal "mo:base/Principal";
import Float "mo:base/Float";
import Time "mo:base/Time";
import Debug "mo:base/Debug";

persistent actor StakingPool {
    type User = Principal;
    type Balance = Float;

    public type StakingRecord = {
        stakedAmount : Balance;
        stakedTime : Time.Time;
        lastClaimedTime : Time.Time;
    };

    // Reward rate (5% APY)
    let rewardRate : Float = 0.05;

    transient let recordMap = Map.Make<User>(Principal.compare);
    var stakingRecords : Map.Map<User, StakingRecord> = recordMap.empty();

    // Stake tokens
    public shared ({ caller }) func stakeTokens(amount : Balance) : async () {
        if (amount <= 0) Debug.trap("Invalid staking amount");

        let currentTime = Time.now();

        let updatedRecord : StakingRecord = switch (recordMap.get(stakingRecords, caller)) {
            case null {
                {
                    stakedAmount = amount;
                    stakedTime = currentTime;
                    lastClaimedTime = currentTime;
                };
            };
            case (?record) {
                {
                    stakedAmount = record.stakedAmount + amount;
                    stakedTime = record.stakedTime;
                    lastClaimedTime = currentTime;
                };
            };
        };

        stakingRecords := recordMap.put(stakingRecords, caller, updatedRecord);
    };

    // Unstake tokens
    public shared ({ caller }) func unstakeTokens() : async Balance {
        let ?record = recordMap.get(stakingRecords, caller) else Debug.trap("No staking record found");

        let rewards = calculateRewards(record);
        let totalAmount = record.stakedAmount + rewards;

        stakingRecords := recordMap.remove(stakingRecords, caller).0;
        totalAmount;
    };

    // Claim rewards
    public shared ({ caller }) func claimRewards() : async Balance {
        let ?record = recordMap.get(stakingRecords, caller) else Debug.trap("No staking record found");

        let rewards = calculateRewards(record);
        if (rewards <= 0) Debug.trap("No rewards to claim");

        let updatedRecord = { record with lastClaimedTime = Time.now() };
        stakingRecords := recordMap.put(stakingRecords, caller, updatedRecord);

        rewards;
    };

    // Query staking balance
    public shared ({ caller }) func getStakingBalance() : async Balance {
        let ?record = recordMap.get(stakingRecords, caller) else return 0.0;

        record.stakedAmount;
    };

    // Calculate APY
    public shared query func calculateAPY() : async Float {
        rewardRate * 100;
    };

    // Helper function to calculate rewards
    func calculateRewards(record : StakingRecord) : Balance {
        let currentTime = Time.now();
        let timeElapsed = Float.fromInt(currentTime - record.lastClaimedTime) / 1_000_000_000;
        let annualReward = record.stakedAmount * rewardRate;
        let rewards = annualReward * (timeElapsed / (365 * 24 * 60 * 60));
        rewards;
    };
};