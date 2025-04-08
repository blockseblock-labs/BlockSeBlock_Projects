import Map "mo:base/OrderedMap";
import Principal "mo:base/Principal";
import Float "mo:base/Float";
import Time "mo:base/Time";
import Text "mo:base/Text";
import Nat "mo:base/Nat";
import Debug "mo:base/Debug";

persistent actor YieldFarming {
    type User = Principal;
    type Token = Principal;
    type Balance = Float;
    type FarmId = Text;

    public type Farm = {
        id : FarmId;
        token : Token;
        totalStaked : Balance;
        rewardRate : Float;
        startTime : Time.Time;
        endTime : Time.Time;
    };

    public type UserFarmData = {
        stakedAmount : Balance;
        lastClaimedTime : Time.Time;
    };

    transient let farmMap = Map.Make<FarmId>(Text.compare);
    var farms : Map.Map<FarmId, Farm> = farmMap.empty();

    transient let userFarmDataMap = Map.Make<User>(Principal.compare);
    var userFarmData : Map.Map<User, Map.Map<FarmId, UserFarmData>> = userFarmDataMap.empty();

    // Counter for generating unique farm IDs
    var nextFarmId : Nat = 0;

    // Create a new farm
    public shared func createFarm(
        token : Token,
        rewardRate : Float,
        durationSeconds : Nat,
    ) : async FarmId {
        if (rewardRate <= 0 or durationSeconds <= 0) {
            Debug.trap("Invalid farm parameters");
        };

        let farmId = "farm-" # Nat.toText(nextFarmId);
        nextFarmId += 1;

        let farm : Farm = {
            id = farmId;
            token = token;
            totalStaked = 0.0;
            rewardRate = rewardRate;
            startTime = Time.now();
            endTime = Time.now() + durationSeconds * 1_000_000_000;
        };

        farms := farmMap.put(farms, farmId, farm);
        farmId;
    };

    // Deposit tokens into a farm
    public shared ({ caller }) func deposit(
        farmId : FarmId,
        amount : Balance,
    ) : async () {
        if (amount <= 0) Debug.trap("Invalid deposit amount");

        let ?farm = farmMap.get(farms, farmId) else Debug.trap("Farm not found");

        if (Time.now() > farm.endTime) Debug.trap("Farm has ended");

        let userFarms = switch (userFarmDataMap.get(userFarmData, caller)) {
            case null farmMap.empty();
            case (?trie) trie;
        };

        let userFarm = switch (farmMap.get(userFarms, farmId)) {
            case null {
                { stakedAmount = amount; lastClaimedTime = Time.now() };
            };
            case (?data) {
                {
                    stakedAmount = data.stakedAmount + amount;
                    lastClaimedTime = Time.now();
                };
            };
        };

        let updatedUserFarms = farmMap.put(userFarms, farmId, userFarm);
        userFarmData := userFarmDataMap.put(userFarmData, caller, updatedUserFarms);

        let updatedFarm = { farm with totalStaked = farm.totalStaked + amount };
        farms := farmMap.put(farms, farmId, updatedFarm);
    };

    // Withdraw tokens from a farm
    public shared ({ caller }) func withdraw(
        farmId : FarmId
    ) : async Balance {
        let ?farm = farmMap.get(farms, farmId) else Debug.trap("Farm not found");

        let ?userFarms = userFarmDataMap.get(userFarmData, caller) else Debug.trap("No staked tokens found");

        let ?userFarm = farmMap.get(userFarms, farmId) else Debug.trap("No staked tokens found");

        if (userFarm.stakedAmount <= 0) Debug.trap("No staked tokens to withdraw");

        let rewards = calculateRewards(farm, userFarm);
        let totalAmount = userFarm.stakedAmount + rewards;

        let updatedUserFarms = farmMap.delete(userFarms, farmId);
        userFarmData := userFarmDataMap.put(userFarmData, caller, updatedUserFarms);

        let updatedFarm = {
            farm with totalStaked = farm.totalStaked - userFarm.stakedAmount
        };
        farms := farmMap.put(farms, farmId, updatedFarm);

        totalAmount;
    };

    // Claim farming rewards
    public shared ({ caller }) func claimRewards(
        farmId : FarmId
    ) : async Balance {
        let ?farm = farmMap.get(farms, farmId) else Debug.trap("Farm not found");

        let ?userFarms = userFarmDataMap.get(userFarmData, caller) else Debug.trap("No staked tokens found");

        let ?userFarm = farmMap.get(userFarms, farmId) else Debug.trap("No staked tokens found");

        let rewards = calculateRewards(farm, userFarm);
        let updatedUserFarm = { userFarm with lastClaimedTime = Time.now() };

        let updatedUserFarms = farmMap.put(userFarms, farmId, updatedUserFarm);
        userFarmData := userFarmDataMap.put(userFarmData, caller, updatedUserFarms);

        rewards;
    };

    // Query farm details
    public shared query func getFarmDetails(farmId : FarmId) : async Farm {
        let ?farm = farmMap.get(farms, farmId) else Debug.trap("Farm not found");
        farm;
    };

    // Calculate farming APY
    public shared query func calculateFarmAPY(farmId : FarmId) : async Float {
        let ?farm = farmMap.get(farms, farmId) else Debug.trap("Farm not found");
        farm.rewardRate * 365 * 100;
    };

    // Helper function to calculate rewards
    func calculateRewards(farm : Farm, userFarm : UserFarmData) : Balance {
        let currentTime = Time.now();
        let timeElapsed = Float.fromInt(currentTime - userFarm.lastClaimedTime) / 1_000_000_000;
        userFarm.stakedAmount * farm.rewardRate * timeElapsed;
    };
};