persistent actor {
    var count : Int = 0;

    public func increment() : async Int {
        count += 1;
        return count;
    };

    public func decrement() : async Int {
        count -= 1;
        return count;
    };

    public func reset() : async Int {
        count := 0;
        return count;
    };
};