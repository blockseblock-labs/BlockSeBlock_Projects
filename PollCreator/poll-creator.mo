import Map "mo:base/OrderedMap";
import Nat "mo:base/Nat";
import Array "mo:base/Array";
import Error "mo:base/Error";
import Principal "mo:base/Principal";

persistent actor {

    // Types ========================================

    /// An answer to a poll.
    public type PollAnswer = {

        /// The answer text.
        answer : Text;

        /// The number of votes this answer has received.
        votes : Nat;
    };

    /// A poll.
    public type Poll = {
        /// The question being asked.
        question : Text;

        /// The principal that created the poll.
        creator : Principal;

        /// The possible answers to the poll.
        answers : [PollAnswer];
    };

    /// The ID of a poll.
    public type PollId = Nat;

    // State ========================================
    private transient let natMap = Map.Make<PollId>(Nat.compare);
    private var polls : Map.Map<PollId, Poll> = natMap.empty();
    private var nextPollId : PollId = 0;

    // Functions ====================================

    /// Starts a new poll
    /// The creator of the poll is the caller of the function.
    /// This function returns the ID of the poll used in other operations.
    public shared (msg) func startPoll(question : Text, answers : [Text]) : async PollId {
        let pollId = nextPollId;
        nextPollId += 1;

        let poll = {
            question;
            creator = msg.caller;
            answers = Array.map<Text, PollAnswer>(answers, func(answer) = { answer; votes = 0 });
        };
        polls := natMap.put(polls, pollId, poll);

        pollId;
    };

    /// Votes on a poll.
    /// Anyone can vote anonymously without any limit on the number of votes.
    public func vote(pollId : PollId, answerIndex : Nat) : async () {
        let ?poll = natMap.get(polls, pollId) else throw Error.reject("Poll not found");
        if (answerIndex >= poll.answers.size()) {
            throw Error.reject("Answer index out of bounds");
        };

        var i = 0;
        func indexed<X>(trans : (Nat, X) -> X) : X -> X = func elem {
            let res = trans(i, elem);
            i += 1;
            res;
        };
        let answers = Array.map<PollAnswer, PollAnswer>(poll.answers, indexed<PollAnswer>(func(i, a) = if (i == answerIndex) ({ a with votes = a.votes + 1 }) else a));

        let (updated_polls, _) = natMap.replace(polls, pollId, { poll with answers });
        polls := updated_polls;
    };

    /// Closes a poll and returns the results.
    public shared (msg) func closePoll(pollId : PollId) : async Poll {
        let (remaining, removed) = natMap.remove(polls, pollId);
        let ?poll = removed else throw Error.reject("Poll not found");
        if (msg.caller != poll.creator) {
            throw Error.reject("Only the poll creator can close the poll");
        };

        polls := remaining;
        poll;
    };

    /// Gets the poll with the given ID.
    public query func getPoll(pollId : PollId) : async Poll {
        let ?poll = natMap.get(polls, pollId) else throw Error.reject("Poll not found");
        poll;
    };
};