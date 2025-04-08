import Float "mo:base/Float";
import Int "mo:base/Int";
import Iter "mo:base/Iter";
import Nat "mo:base/Nat";
import Option "mo:base/Option";
import OrderedMap "mo:base/OrderedMap";
import Principal "mo:base/Principal";
import Result "mo:base/Result";
import Time "mo:base/Time";
import Trie "mo:base/Trie";

/// Health data tracker actor for heart rate, steps, weight tracking
persistent actor HealthDataTracker {
    type Result<T, E> = Result.Result<T, E>;
    type Time = Time.Time;

    type HeartRate = Nat;
    type Steps = Nat;
    type WeightKg = Float.Float;

    type HealthData = HealthTracking.HealthData;
    type HeartRateData = HeartRateTracking.HeartRateData;
    type StepData = StepTracking.StepData;
    type WeightData = WeightTracking.WeightData;

    private transient let timeMap = OrderedMap.Make<Time>(Int.compare);

    public type AggregationPeriod = {
        #Daily;
        #Weekly;
        #Monthly;
    };

    public type AddSampleError = {
        #ExpiredSample;
        #DuplicateSample;
        #InvalidHeartRate;
        #InvalidSteps;
        #InvalidWeight;
        #InvalidTimestamp;
    };

    public type RangeQueryError = {
        #InvalidDateRange;
    };

    let oneDayInNanoseconds = 86400_000_000_000;
    let rawSampleRetentionNanoseconds = 7 * oneDayInNanoseconds;
    let dailyAggregateRetentionNanoseconds = 90 * oneDayInNanoseconds;
    let weeklyAggregateRetentionNanoseconds = 366 * oneDayInNanoseconds;
    let maxEntriesToRemoveInBatch = 10;

    // These maximum values primarily serve to detect reversal of the timestamp and value parameters
    // They are intended to be much higher than any reasonable value, and much lower than any timestamp
    let maxValidHeartRate = 300;
    let maxValidSteps = 100_000;
    let maxValidWeight = 1_000.0;

    private var principalToHealthData : Trie.Trie<Principal, HealthData> = Trie.empty();

    module HealthTracking {
        public type HealthData = {
            heartRate : HeartRateData;
            steps : StepData;
            weight : WeightData;
        };

        func create() : HealthData {
            {
                heartRate = HeartRateTracking.create();
                steps = StepTracking.create();
                weight = WeightTracking.create();
            };
        };

        public func getOrCreateHealthData(caller : Principal) : HealthData {
            let key = keyPrincipal(caller);

            // Attempt to retrieve existing data, or initialize and insert it if not found
            switch (Trie.get<Principal, HealthData>(principalToHealthData, key, Principal.equal)) {
                case (?healthData) healthData;
                case null {
                    let newData = create();
                    principalToHealthData := Trie.put<Principal, HealthData>(principalToHealthData, key, Principal.equal, newData).0;
                    newData;
                };
            };
        };
    };

    //
    // Heart Rate Tracking
    //

    module HeartRateTracking {
        public type Aggregate = {
            avg : HeartRate;
            max : HeartRate;
            min : HeartRate;
        };

        type AggregateAccumulator = {
            sum : Nat;
            count : Nat;
            max : HeartRate;
            min : HeartRate;
        };

        type SampleMap = OrderedMap.Map<Time, HeartRate>;
        type AggregateMap = OrderedMap.Map<Time, AggregateAccumulator>;

        public type HeartRateData = {
            /// Raw heart rate samples recorded from the source.
            /// This map retains data for up to 7 days.
            var samples : SampleMap;

            /// Aggregated heart rate data rolled up per day.
            /// Derived from raw samples and retained long-term.
            var daily : AggregateMap;

            /// Aggregated heart rate data rolled up per week.
            /// Derived from raw samples and retained long-term.
            var weekly : AggregateMap;

            /// Aggregated heart rate data rolled up per month.
            /// Derived from raw samples and retained indefinitely.
            var monthly : AggregateMap;
        };

        public func create() : HeartRateData {
            {
                var samples = timeMap.empty();
                var daily = timeMap.empty();
                var weekly = timeMap.empty();
                var monthly = timeMap.empty();
            };
        };

        public func addSample(caller : Principal, timestamp : Time, heartRate : HeartRate, now : Time) : Result<(), AddSampleError> {
            if (heartRate > maxValidHeartRate) {
                return #err(#InvalidHeartRate);
            };

            // Validate timestamp (e.g., no timestamps far in the future)
            if (timestamp > now + rawSampleRetentionNanoseconds) {
                return #err(#InvalidTimestamp);
            };

            // Reject samples that could have already been received and expired
            if (timestamp < now - rawSampleRetentionNanoseconds) {
                return #err(#ExpiredSample);
            };

            let h = HealthTracking.getOrCreateHealthData(caller).heartRate;

            h.samples := deleteExpiredData(h.samples, rawSampleRetentionNanoseconds);
            h.daily := deleteExpiredData(h.daily, dailyAggregateRetentionNanoseconds);
            h.weekly := deleteExpiredData(h.weekly, weeklyAggregateRetentionNanoseconds);
            // monthly data is retained indefinitely

            // Add the raw sample with deduplication
            let (samples, old) = timeMap.replace(h.samples, timestamp, heartRate);
            if (old != null) {
                return #err(#DuplicateSample);
            };

            h.samples := samples;
            h.daily := updateAggregate(h.daily, timeToStartOfDay(timestamp), heartRate);
            h.weekly := updateAggregate(h.weekly, timeToStartOfWeek(timestamp), heartRate);
            h.monthly := updateAggregate(h.monthly, timeToStartOfMonth(timestamp), heartRate);
            #ok();
        };

        public func getSamples(caller : Principal, start : Time, end : Time) : Result<[(Time, HeartRate)], RangeQueryError> {
            if (start > end) {
                return #err(#InvalidDateRange);
            };

            let key = keyPrincipal(caller);
            let maybeHealthData = Trie.get<Principal, HealthData>(principalToHealthData, key, Principal.equal);

            // If health data exists, query the raw samples; otherwise, return an empty array.
            let samples = Option.getMapped<HealthData, [(Time, HeartRate)]>(
                maybeHealthData,
                func(userData) = filterRawSamples(userData.heartRate.samples, start, end),
                [],
            );
            #ok samples;
        };

        /// Queries the raw heart rate samples within a specified date range.
        /// `start` is inclusive and `end` is exclusive.
        func filterRawSamples(
            samples : SampleMap,
            start : Time,
            end : Time,
        ) : [(Time, HeartRate)] {
            Iter.toArray<(Time, HeartRate)>(
                Iter.filter<(Time, HeartRate)>(
                    timeMap.entries(samples),
                    func(entry) = entry.0 >= start and entry.0 < end,
                )
            );
        };

        public func getAggregates(caller : Principal, start : Time, end : Time, period : AggregationPeriod) : Result<[(Time, Aggregate)], RangeQueryError> {
            if (start > end) {
                return #err(#InvalidDateRange);
            };

            let key = keyPrincipal(caller);
            let maybeHealthData = Trie.get<Principal, HealthData>(principalToHealthData, key, Principal.equal);

            // If health data exists, return the aggregates; otherwise, return an empty array.
            let samples = Option.getMapped<HealthData, [(Time, Aggregate)]>(
                maybeHealthData,
                func(userData) = filterMapAggregates(userData.heartRate, start, end, period),
                [],
            );
            #ok samples;
        };

        /// Queries aggregated heart rate data within the specified date range.
        /// `start` is inclusive and `end` is exclusive.
        func filterMapAggregates(
            heartRateData : HeartRateData,
            start : Time,
            end : Time,
            period : AggregationPeriod,
        ) : [(Time, Aggregate)] {
            let map = switch period {
                case (#Daily) { heartRateData.daily };
                case (#Weekly) { heartRateData.weekly };
                case (#Monthly) { heartRateData.monthly };
            };

            let filterEntry : ((Time, AggregateAccumulator)) -> Bool = func(entry) = entry.0 >= start and entry.0 < end;

            let transformAggregate : ((Time, AggregateAccumulator)) -> ((Time, Aggregate)) = func(entry) = (
                entry.0,
                {
                    avg = if (entry.1.count > 0) entry.1.sum / entry.1.count else 0;
                    max = entry.1.max;
                    min = entry.1.min;
                },
            );

            Iter.toArray<(Time, Aggregate)>(
                Iter.map<(Time, AggregateAccumulator), (Time, Aggregate)>(
                    Iter.filter<(Time, AggregateAccumulator)>(
                        timeMap.entries(map),
                        filterEntry,
                    ),
                    transformAggregate,
                )
            );
        };

        func updateAggregate(map : AggregateMap, timestamp : Time, heartRate : HeartRate) : AggregateMap {
            let previous = Option.get(timeMap.get(map, timestamp), { sum = 0; count = 0; max = 0; min = 0 });
            let updated = {
                sum = previous.sum + heartRate;
                count = previous.count + 1;
                max = Nat.max(previous.max, heartRate);
                min = Nat.min(previous.min, heartRate);
            };

            timeMap.put(map, timestamp, updated);
        };
    };

    //
    // Step Tracking
    //
    module StepTracking {
        type SampleMap = OrderedMap.Map<Time, Steps>;
        type AggregateMap = OrderedMap.Map<Time, Steps>;

        public type StepData = {
            var samples : SampleMap;
            var daily : AggregateMap;
            var weekly : AggregateMap;
            var monthly : AggregateMap;
        };

        public func create() : StepData {
            {
                var samples = timeMap.empty();
                var daily = timeMap.empty();
                var weekly = timeMap.empty();
                var monthly = timeMap.empty();
            };
        };

        public func addSample(caller : Principal, timestamp : Time, value : Steps, now : Time) : Result<(), AddSampleError> {
            if (value > maxValidSteps) {
                return #err(#InvalidSteps);
            };

            // Validate timestamp (e.g., no timestamps far in the future)
            if (timestamp > now + rawSampleRetentionNanoseconds) {
                return #err(#InvalidTimestamp);
            };

            // Reject samples that could have already been received and expired
            if (timestamp < now - rawSampleRetentionNanoseconds) {
                return #err(#ExpiredSample);
            };

            let steps = HealthTracking.getOrCreateHealthData(caller).steps;

            steps.samples := deleteExpiredData(steps.samples, rawSampleRetentionNanoseconds);
            steps.daily := deleteExpiredData(steps.daily, dailyAggregateRetentionNanoseconds);
            steps.weekly := deleteExpiredData(steps.weekly, weeklyAggregateRetentionNanoseconds);
            // monthly data is retained indefinitely

            let (samples, old) = timeMap.replace(steps.samples, timestamp, value);
            if (old != null) {
                return #err(#DuplicateSample);
            };

            steps.samples := samples;
            steps.daily := updateAggregate(steps.daily, timeToStartOfDay(timestamp), value);
            steps.weekly := updateAggregate(steps.weekly, timeToStartOfWeek(timestamp), value);
            steps.monthly := updateAggregate(steps.monthly, timeToStartOfMonth(timestamp), value);

            #ok();
        };

        func updateAggregate(map : AggregateMap, timestamp : Time, steps : Steps) : AggregateMap {
            let totalStepsForPeriod = Option.get(timeMap.get(map, timestamp), 0) + steps;
            timeMap.put(map, timestamp, totalStepsForPeriod);
        };

        public func getSamples(caller : Principal, start : Time, end : Time) : Result<[(Time, Steps)], RangeQueryError> {
            if (start > end) {
                return #err(#InvalidDateRange);
            };

            let key = keyPrincipal(caller);
            let maybeHealthData = Trie.get<Principal, HealthData>(principalToHealthData, key, Principal.equal);

            // If health data exists, query the raw samples; otherwise, return an empty array.
            let samples = Option.getMapped<HealthData, [(Time, Steps)]>(
                maybeHealthData,
                func(userData) = filterRawSamples(userData.steps.samples, start, end),
                [],
            );
            #ok samples;
        };

        /// Queries the raw step samples within a specified date range.
        /// `start` is inclusive and `end` is exclusive.
        func filterRawSamples(
            samples : SampleMap,
            start : Time,
            end : Time,
        ) : [(Time, Steps)] {
            Iter.toArray<(Time, Steps)>(
                Iter.filter<(Time, Steps)>(
                    timeMap.entries(samples),
                    func(entry) = entry.0 >= start and entry.0 < end,
                )
            );
        };

        public func getAggregates(caller : Principal, start : Time, end : Time, period : AggregationPeriod) : Result<[(Time, Steps)], RangeQueryError> {
            if (start > end) {
                return #err(#InvalidDateRange);
            };

            let key = keyPrincipal(caller);
            let maybeHealthData = Trie.get<Principal, HealthData>(principalToHealthData, key, Principal.equal);

            // If health data exists, query the raw samples; otherwise, return an empty array.
            let samples = Option.getMapped<HealthData, [(Time, Steps)]>(
                maybeHealthData,
                func(userData) = filterAggregates(userData.steps, start, end, period),
                [],
            );
            #ok samples;
        };

        /// Queries the aggregated steps within a specified date range.
        /// `start` is inclusive and `end` is exclusive.
        func filterAggregates(
            steps : StepData,
            start : Time,
            end : Time,
            period : AggregationPeriod,
        ) : [(Time, Steps)] {
            // Select the appropriate AggregateMap based on the aggregateType
            let map = switch period {
                case (#Daily) { steps.daily };
                case (#Weekly) { steps.weekly };
                case (#Monthly) { steps.monthly };
            };

            Iter.toArray<(Time, Steps)>(
                Iter.filter<(Time, Steps)>(
                    timeMap.entries(map),
                    func(entry) = entry.0 >= start and entry.0 < end,
                )
            );
        };
    };

    module WeightTracking {
        type RawSampleMap = OrderedMap.Map<Time, WeightKg>;

        /// Accumulated data for a period (daily, weekly, monthly)
        public type Aggregate = {
            first : { time : Time; weight : WeightKg }; // First weight in the period and its timestamp
            last : { time : Time; weight : WeightKg }; // Last weight in the period and its timestamp
            max : WeightKg; // Maximum weight in the period
            min : WeightKg; // Minimum weight in the period
            count : Nat; // Number of weight measurements
        };
        type AggregateMap = OrderedMap.Map<Time, Aggregate>;

        public type WeightData = {
            var samples : RawSampleMap;
            var daily : AggregateMap;
            var weekly : AggregateMap;
            var monthly : AggregateMap;
        };

        public func create() : WeightData {
            {
                var samples = timeMap.empty();
                var daily = timeMap.empty();
                var weekly = timeMap.empty();
                var monthly = timeMap.empty();
            };
        };

        public func addSample(caller : Principal, timestamp : Time, weight : WeightKg, now : Time) : Result<(), AddSampleError> {
            if (weight > maxValidWeight) {
                return #err(#InvalidWeight);
            };

            // Validate timestamp (e.g., no timestamps far in the future)
            if (timestamp > now + rawSampleRetentionNanoseconds) {
                return #err(#InvalidTimestamp);
            };

            // Reject samples that could have already been received and expired
            if (timestamp < now - rawSampleRetentionNanoseconds) {
                return #err(#ExpiredSample);
            };

            let w = HealthTracking.getOrCreateHealthData(caller).weight;

            w.samples := deleteExpiredData(w.samples, rawSampleRetentionNanoseconds);
            w.daily := deleteExpiredData(w.daily, dailyAggregateRetentionNanoseconds);
            w.weekly := deleteExpiredData(w.weekly, weeklyAggregateRetentionNanoseconds);

            let (samples, old) = timeMap.replace(w.samples, timestamp, weight);
            if (old != null) {
                return #err(#DuplicateSample);
            };

            w.samples := samples;
            w.daily := updateAggregate(w.daily, timeToStartOfDay(timestamp), weight);
            w.weekly := updateAggregate(w.weekly, timeToStartOfWeek(timestamp), weight);
            w.monthly := updateAggregate(w.monthly, timeToStartOfMonth(timestamp), weight);

            #ok();
        };

        func updateAggregate(
            aggregates : AggregateMap,
            time : Time,
            weight : WeightKg,
        ) : AggregateMap {
            let updated = switch (timeMap.get(aggregates, time)) {
                case (?current) {
                    {
                        first = if (time < current.first.time) {
                            {
                                time;
                                weight;
                            };
                        } else {
                            current.first;
                        };
                        last = if (time > current.last.time) {
                            {
                                time;
                                weight;
                            };
                        } else {
                            current.last;
                        };
                        max = Float.max(current.max, weight);
                        min = Float.min(current.min, weight);
                        count = current.count + 1;
                    };
                };
                case null {
                    // Initialize a new aggregate if none exists for the time period
                    {
                        first = { time; weight };
                        last = { time; weight };
                        max = weight;
                        min = weight;
                        count = 1;
                    };
                };
            };

            // Insert the updated aggregate back into the map
            timeMap.put(aggregates, time, updated);
        };

        public func getSamples(caller : Principal, start : Time, end : Time) : Result<[(Time, WeightKg)], RangeQueryError> {
            if (start > end) {
                return #err(#InvalidDateRange);
            };

            let key = keyPrincipal(caller);
            let maybeHealthData = Trie.get<Principal, HealthData>(principalToHealthData, key, Principal.equal);

            // If health data exists, query the raw samples; otherwise, return an empty array.
            let samples = Option.getMapped<HealthData, [(Time, WeightKg)]>(
                maybeHealthData,
                func(userData) = filterRawSamples(userData.weight.samples, start, end),
                [],
            );
            #ok samples;
        };

        /// Queries the raw weight samples within a specified date range.
        /// `start` is inclusive and `end` is exclusive.
        func filterRawSamples(
            samples : RawSampleMap,
            start : Time,
            end : Time,
        ) : [(Time, WeightKg)] {
            Iter.toArray<(Time, WeightKg)>(
                Iter.filter<(Time, WeightKg)>(
                    timeMap.entries(samples),
                    func(entry) = entry.0 >= start and entry.0 < end,
                )
            );
        };

        public func getAggregates(caller : Principal, start : Time, end : Time, period : AggregationPeriod) : Result<[(Time, Aggregate)], RangeQueryError> {
            if (start > end) {
                return #err(#InvalidDateRange);
            };

            let key = keyPrincipal(caller);
            let maybeHealthData = Trie.get<Principal, HealthData>(principalToHealthData, key, Principal.equal);

            // If health data exists, return the aggregates for the specified range; otherwise, return an empty array.
            let aggregates = Option.getMapped<HealthData, [(Time, Aggregate)]>(
                maybeHealthData,
                func(userData) = filterAggregates(userData.weight, start, end, period),
                [],
            );
            #ok aggregates;
        };

        /// Queries the aggregated heart rate data within a specified date range.
        /// `start` is inclusive and `end` is exclusive.
        func filterAggregates(
            weightData : WeightData,
            start : Time,
            end : Time,
            period : AggregationPeriod,
        ) : [(Time, Aggregate)] {
            let map = switch period {
                case (#Daily) { weightData.daily };
                case (#Weekly) { weightData.weekly };
                case (#Monthly) { weightData.monthly };
            };

            Iter.toArray<(Time, Aggregate)>(
                Iter.filter<(Time, Aggregate)>(
                    timeMap.entries(map),
                    func(entry) = entry.0 >= start and entry.0 < end,
                )
            );
        };
    };

    //
    // Heart rate tracking canister interface
    //

    /// Add a heart rate sample for the caller at the specified timestamp.
    public shared ({ caller }) func addHeartRateSample(timestamp : Time, heartRate : HeartRate) : async Result<(), AddSampleError> {
        HeartRateTracking.addSample(caller, timestamp, heartRate, getCurrentTime());
    };

    /// Get the heart rate samples for the caller within the specified date range.
    /// `start` is inclusive and `end` is exclusive.
    public query ({ caller }) func getHeartRateRawSamples(start : Time, end : Time) : async Result<[(Time, HeartRate)], RangeQueryError> {
        HeartRateTracking.getSamples(caller, start, end);
    };

    /// Get the daily heart rate aggregates for the caller within the specified date range.
    /// `start` is inclusive and `end` is exclusive.
    public query ({ caller }) func getHeartRateAggregates(start : Time, end : Time, period : AggregationPeriod) : async Result<[(Time, HeartRateTracking.Aggregate)], RangeQueryError> {
        HeartRateTracking.getAggregates(caller, start, end, period);
    };

    //
    // Step tracking canister interface
    //

    /// Add a step sample for the caller at the specified timestamp.
    public shared ({ caller }) func addStepSample(timestamp : Time, steps : Steps) : async Result<(), AddSampleError> {
        StepTracking.addSample(caller, timestamp, steps, getCurrentTime());
    };

    /// Get the step samples for the caller within the specified date range.
    /// `start` is inclusive and `end` is exclusive.
    public query ({ caller }) func getStepSamples(start : Time, end : Time) : async Result<[(Time, Steps)], RangeQueryError> {
        StepTracking.getSamples(caller, start, end);
    };

    /// Get the step aggregates for the caller within the specified date range.
    /// `start` is inclusive and `end` is exclusive.
    public query ({ caller }) func getStepAggregates(start : Time, end : Time, period : AggregationPeriod) : async Result<[(Time, Steps)], RangeQueryError> {
        StepTracking.getAggregates(caller, start, end, period);
    };

    //
    // Weight tracking canister interface
    //

    /// Add a weight sample for the caller at the specified timestamp.
    public shared (msg) func addWeightSample(timestamp : Time, kg : WeightKg) : async Result<(), AddSampleError> {
        WeightTracking.addSample(msg.caller, timestamp, kg, getCurrentTime());
    };

    /// Get the weight samples for the caller within the specified date range.
    /// `start` is inclusive and `end` is exclusive.
    public query (msg) func getWeightSamples(start : Time, end : Time) : async Result<[(Time, WeightKg)], RangeQueryError> {
        WeightTracking.getSamples(msg.caller, start, end);
    };

    /// Get the weight aggregates for the caller within the specified date range.
    /// `start` is inclusive and `end` is exclusive.
    public query (msg) func getWeightAggregates(start : Time, end : Time, period : AggregationPeriod) : async Result<[(Time, WeightTracking.Aggregate)], RangeQueryError> {
        WeightTracking.getAggregates(msg.caller, start, end, period);
    };

    //
    // Helper functions
    //

    /// Helper function to create a trie key for the principal.
    private func keyPrincipal(p : Principal) : Trie.Key<Principal> {
        { hash = Principal.hash(p); key = p };
    };

    private func timeToStartOfDay(time : Time) : Time {
        time - (time % oneDayInNanoseconds);
    };

    /// Placeholder method for getting the start of a week from an arbitrate time
    /// Replace with something real when there's a date/time library
    private func timeToStartOfWeek(time : Time) : Time {
        time - (time % (7 * oneDayInNanoseconds));
    };

    /// Placeholder method for getting the start of a month from an arbitrate time
    /// Replace with something real when there's a date/time library
    private func timeToStartOfMonth(time : Time) : Time {
        time - (time % (30 * oneDayInNanoseconds));
    };

    /// Delete up to 'maxEntriesToRemoveInBatch' entries older than the retention period
    private func deleteExpiredData<V>(input : OrderedMap.Map<Time, V>, retentionNanoseconds : Nat) : OrderedMap.Map<Time, V> {
        var result = input;

        let threshold = getCurrentTime() - retentionNanoseconds;
        var removedCount = 0;

        // Remove entries older than the threshold in batches
        label outer while (removedCount < maxEntriesToRemoveInBatch) {
            switch (timeMap.minEntry(result)) {
                case (?minEntry) {
                    // minEntry returns a tuple (key, value) -> remove the oldest entry if it's older than the threshold
                    let (timestamp, _) = minEntry;
                    if (timestamp < threshold) {
                        result := timeMap.delete(result, timestamp);
                        removedCount += 1;
                    } else {
                        // No more old entries to remove
                        break outer;
                    };
                };
                case null {
                    // No more entries to remove
                    break outer;
                };
            };
        };
        result;
    };

    // For testing purposes

    var overrideTimeNow : ?Time = null;

    func getCurrentTime() : Time {
        switch overrideTimeNow {
            case (?time) { time };
            case null { Time.now() };
        };
    };

    public shared func overrideCurrentTime(time : Time) : async () {
        overrideTimeNow := ?time;
    };
};