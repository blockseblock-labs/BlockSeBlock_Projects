import Error "mo:base/Error";
import Int "mo:base/Int";
import List "mo:base/List";
import Time "mo:base/Time";
import Timer "mo:base/Timer";
import Principal "mo:base/Principal";

persistent actor {
    type Id = Blob;
    type Worker = shared () -> ();
    type Expiration = {
        id : Id;
        owner : Principal;
        worker : Worker;
        deadline : Time.Time;
        systemTimerId : Timer.TimerId;
    };

    private func drop(id : Id, owner : Principal) {
        func isMatching(expiration : Expiration) : Bool {
            expiration.id == id and expiration.owner == owner
        };
        let matching = List.filter<Expiration>(expirations, isMatching);
        List.iterate<Expiration>(
            matching,
            func expiration {
                Timer.cancelTimer(expiration.systemTimerId);
            },
        );
        expirations := List.filter<Expiration>(expirations, func expiration { not isMatching(expiration) });
    };

    private func restart<system>(expiration : Expiration) : Expiration {
        let due = expiration.deadline - Time.now();
        let systemTimerId = Timer.setTimer<system>(
            #nanoseconds(if (due < 0) 0 else Int.abs(due)),
            func() : async () {
                drop(expiration.id, expiration.owner);
                expiration.worker(); // one-way function call, aka fire-and-forget call
            },
        );
        { expiration with systemTimerId };
    };

    var expirations : List.List<Expiration> = null;

    // this is a port of `List.map` to the `<system>` demand
    private func map<system>(list : List.List<Expiration>, mapping : <system>(Expiration) -> Expiration) : List.List<Expiration> {
        switch list {
            case null null;
            case (?(head, tail)) ?(mapping<system>(head), map<system>(tail, mapping));
        };
    };

    expirations := map<system>(expirations, restart);

    var count : Nat = 0;

    public shared ({ caller }) func startTimer(duration : Timer.Duration, callback : Worker) : async Id {
        count += 1;
        let id = to_candid (caller, duration, count);
        let systemTimerId = Timer.setTimer<system>(
            duration,
            func() : async () {
                drop(id, caller);
                callback();
            },
        );
        let nanos = switch duration {
            case (#nanoseconds n) n;
            case (#seconds s) s * 1_000_000_000;
        };
        let newExpiration = {
            id;
            owner = caller;
            worker = callback;
            deadline = Time.now() + nanos;
            systemTimerId;
        };
        expirations := List.push(newExpiration, expirations);
        id;
    };

    public shared ({ caller }) func cancelTimer(id : Id) : async () {
        let countBefore = List.size(expirations);
        drop(id, caller);
        if (countBefore == List.size(expirations)) {
            throw Error.reject("Timer not set");
        };
    };
};