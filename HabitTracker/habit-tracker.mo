import Array "mo:base/Array";
import Text "mo:base/Text";
import Time "mo:base/Time";
import { abs } = "mo:base/Int";
import { setTimer; recurringTimer } = "mo:base/Timer";
import Result "mo:base/Result";
import Principal "mo:base/Principal";
import Trie "mo:base/Trie";

persistent actor {
    let daySeconds : Nat = 24 * 60 * 60;

    type Result<T, E> = Result.Result<T, E>;
    type Time = Time.Time;

    public type DailyHabit = {
        activity : Text;
        var completed : Bool;
    };

    public type HabitStatus = {
        completed : [Text];
        pending : [Text];
    };

    private var habitTracker : Trie.Trie<Principal, [DailyHabit]> = Trie.empty();

    private func keyPrincipal(p : Principal) : Trie.Key<Principal> {
        { hash = Principal.hash(p); key = p };
    };

    private func getHabitsForPrincipal(p : Principal) : Result<[DailyHabit], Text> {
        switch (Trie.find(habitTracker, keyPrincipal(p), Principal.equal)) {
            case null #err("Principal with id " # debug_show (p) # " does not exist");
            case (?habits) #ok habits;
        };
    };

    private func resetDailyHabits() : async () {
        for ((k, v) in Trie.iter(habitTracker)) {
            let resetHabits = Array.map<DailyHabit, DailyHabit>(v, func(habit) { { habit with var completed = false } });
            habitTracker := Trie.put(habitTracker, keyPrincipal(k), Principal.equal, resetHabits).0;
        };
    };

    ignore setTimer<system>(
        #seconds(daySeconds - (abs(Time.now()) / 1_000_000_000) % daySeconds),
        func() : async () {
            ignore recurringTimer<system>(#seconds daySeconds, resetDailyHabits);
            await resetDailyHabits();
        },
    );

    public shared (msg) func addHabit(activity : Text) : async Result<(), Text> {
        let habits = switch (getHabitsForPrincipal(msg.caller)) {
            case (#err _) {
                let emptyHabits : [DailyHabit] = [];

                habitTracker := Trie.put(
                    habitTracker,
                    keyPrincipal(msg.caller),
                    Principal.equal,
                    emptyHabits,
                ).0;
                emptyHabits;
            };
            case (#ok habits) {
                switch (Array.find<DailyHabit>(habits, func(h) { Text.equal(h.activity, activity) })) {
                    case null habits;
                    case (?_) return #err "Habit already exists";
                };
            };
        };

        let updatedHabits = Array.append<DailyHabit>(habits, [{ activity; var completed = false }]);
        habitTracker := Trie.put(habitTracker, keyPrincipal(msg.caller), Principal.equal, updatedHabits).0;
        #ok;
    };

    public shared (msg) func removeHabit(activity : Text) : async Result<(), Text> {
        switch (getHabitsForPrincipal(msg.caller)) {
            case (#err e) #err e;
            case (#ok habits) {
                switch (Array.find<DailyHabit>(habits, func h { Text.equal(h.activity, activity) })) {
                    case null #err "Habit does not exist";
                    case (?_) {
                        let updatedHabits = Array.filter<DailyHabit>(
                            habits,
                            func h {
                                not Text.equal(h.activity, activity);
                            },
                        );
                        habitTracker := Trie.put(habitTracker, keyPrincipal(msg.caller), Principal.equal, updatedHabits).0;
                        #ok;
                    };
                };
            };
        };
    };

    public shared (msg) func completeHabit(activity : Text) : async Result<(), Text> {
        switch (getHabitsForPrincipal(msg.caller)) {
            case (#err e) #err e;
            case (#ok habits) {
                switch (Array.find<DailyHabit>(habits, func h { Text.equal(h.activity, activity) })) {
                    case null #err("Habit with name " # debug_show activity # " does not exist");
                    case (?entry) {
                        entry.completed := true;
                        #ok;
                    };
                };
            };
        };
    };

    public shared (msg) func getHabits() : async Result<HabitStatus, Text> {
        switch (getHabitsForPrincipal(msg.caller)) {
            case (#err e) (#err e);
            case (#ok habits) {
                let completed = Array.map<DailyHabit, Text>(
                    Array.filter<DailyHabit>(habits, func h { h.completed }),
                    func h { h.activity },
                );
                let pending = Array.map<DailyHabit, Text>(
                    Array.filter<DailyHabit>(habits, func h { not h.completed }),
                    func h { h.activity },
                );
                #ok { completed; pending };
            };
        };
    };
};