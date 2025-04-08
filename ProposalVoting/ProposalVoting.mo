import Nat "mo:base/Nat";
import Map "mo:base/OrderedMap";
import Text "mo:base/Text";
import Result "mo:base/Result";
import Principal "mo:base/Principal";

persistent actor {
    type Vote = {
        #For;
        #Against;
    };

    type Map<K, V> = Map.Map<K, V>;

    type Result<T, E> = Result.Result<T, E>;

    type Votes = Map<Principal, Vote>;

    type Proposal = {
        author : Principal;
        description : Text;
        votes : Votes;
    };

    type Tally = {
        forCount : Nat;
        againstCount : Nat;
    };

    private transient let natMap = Map.Make<Nat>(Nat.compare);

    private transient let principalMap = Map.Make<Principal>(Principal.compare);

    private var idCounter : Nat = 0;

    private var proposals : Map<Nat, Proposal> = natMap.empty();

    public shared (msg) func submitProposal(description : Text) : async Result<Nat, Text> {
        if (Principal.isAnonymous(msg.caller)) {
            return #err("Anonymous users cannot create profiles");
        };
        let proposal : Proposal = {
            author = msg.caller;
            description;
            votes : Map<Principal, Vote> = principalMap.empty();
        };
        let id = idCounter;
        proposals := natMap.put(proposals, id, proposal);
        idCounter += 1;
        #ok(id);
    };

    public shared (msg) func voteForProposal(id : Nat) : async Result<Text, Text> {
        switch (vote(msg.caller, id, #For)) {
            case (#err(error)) #err(error);
            case (#ok) #ok(Principal.toText(msg.caller) # " voted for proposal " # Nat.toText(id));
        };
    };

    public shared (msg) func voteAgainstProposal(id : Nat) : async Result<Text, Text> {
        switch (vote(msg.caller, id, #Against)) {
            case (#err(error)) #err(error);
            case (#ok) #ok(Principal.toText(msg.caller) # " voted against proposal " # Nat.toText(id));
        };
    };

    private func vote(principal : Principal, id : Nat, value : Vote) : Result<(), Text> {
        if (Principal.isAnonymous(principal)) {
            return #err("Anonymous users cannot create profiles");
        };
        switch (natMap.get(proposals, id)) {
            case null {
                #err("Proposal not found");
            };
            case (?proposal) {
                if (principalMap.get(proposal.votes, principal) != null) {
                    return #err("Already voted for proposal");
                };
                let updatedVotes = principalMap.put(proposal.votes, principal, value);
                let updatedProposal = { proposal with votes = updatedVotes };
                proposals := natMap.put(proposals, id, updatedProposal);
                #ok;
            };
        };
    };

    public shared query func getVoteTally(id : Nat) : async Result<Tally, Text> {
        switch (natMap.get(proposals, id)) {
            case null {
                #err("Proposal not found");
            };
            case (?proposal) {
                let emptyTally : Tally = {
                    forCount = 0;
                    againstCount = 0;
                };
                func tallyVotes(_ : Principal, vote : Vote, tally : Tally) : Tally {
                    switch (vote) {
                        case (#For) {
                            { tally with forCount = tally.forCount + 1 };
                        };
                        case (#Against) {
                            { tally with againstCount = tally.againstCount + 1 };
                        };
                    };
                };
                #ok(principalMap.foldRight(proposal.votes, emptyTally, tallyVotes));
            };
        };
    };
};