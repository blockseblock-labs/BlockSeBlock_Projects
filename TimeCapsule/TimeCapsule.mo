import Time "mo:base/Time";
import Trie "mo:base/Trie";
import Result "mo:base/Result";
import Hash "mo:base/Hash";
import Nat "mo:base/Nat";
import Text "mo:base/Text";
import Principal "mo:base/Principal";

persistent actor {
    type Time = Int;

    type Capsule = {
        author : Principal;
        releaseTime : Time;
        content : Text;
    };

    type Key<T> = Trie.Key<T>;

    type Capsules = Trie.Trie<Nat, Capsule>;

    type Result<T, E> = Result.Result<T, E>;

    private var idCounter : Nat = 0;

    private var capsules : Capsules = Trie.empty();

    func customHash(n : Nat) : Hash.Hash {
        Text.hash(Nat.toText(n));
    };

    func key(id : Nat) : Key<Nat> = { hash = customHash id; key = id };

    public shared ({ caller }) func storeTimeCapsule(releaseTime : Time, content : Text) : async Result<Nat, Text> {
        if (Principal.isAnonymous(caller)) {
            return #err "Anonymous users cannot store time capsules";
        };
        let capsule : Capsule = {
            author = caller;
            releaseTime;
            content;
        };
        let id = idCounter;
        capsules := Trie.put(capsules, key id, Nat.equal, capsule).0;
        idCounter += 1;
        #ok id;
    };

    public shared query func readTimeCapsule(id : Nat) : async Result<Capsule, Text> {
        let capsule = Trie.get(capsules, key id, Nat.equal);
        switch capsule {
            case null {
                #err "Time capsule does not exist";
            };
            case (?capsule) {
                if (Time.now() < capsule.releaseTime) {
                    #err "Time capsule is still locked!";
                } else {
                    #ok capsule;
                };
            };
        };
    };
};