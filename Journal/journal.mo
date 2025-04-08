import Text "mo:base/Text";
import Trie "mo:base/Trie";
import Nat "mo:base/Nat";
import Hash "mo:base/Hash";
import Result "mo:base/Result";

import Principal "mo:base/Principal";

persistent actor {

    type Entry = {
        title : Text;
        body : Text;
    };

    type Journal = Trie.Trie<Nat, Entry>;

    type Journals = Trie.Trie<Principal, Journal>;

    type Key<K> = Trie.Key<K>;

    type Result<T, E> = Result.Result<T, E>;

    private var journals : Journals = Trie.empty();
    private var idCounter : Nat = 0;

    func customHash(n : Nat) : Hash.Hash {
        Text.hash(Nat.toText(n));
    };

    func entryKey(id : Nat) : Key<Nat> { { hash = customHash id; key = id } };

    func journalKey(principal : Principal) : Key<Principal> {
        { hash = Principal.hash principal; key = principal };
    };

    public shared (msg) func startJournal() : async Result<Text, Text> {
        if (Principal.isAnonymous(msg.caller)) {
            return #err("Anonymous users cannot create journals");
        };
        if (Trie.get(journals, journalKey(msg.caller), Principal.equal) != null) {
            return #err("Journal already exists");
        };

        let journal = Trie.empty();
        journals := Trie.put(journals, journalKey(msg.caller), Principal.equal, journal).0;
        #ok("Journal created for " # Principal.toText(msg.caller));
    };

    public shared query (msg) func viewEntries() : async Result<[(Nat, Entry)], Text> {
        let journal = Trie.get(journals, journalKey(msg.caller), Principal.equal);
        switch (journal) {
            case (null) { #err("Journal not found") };
            case (?journal) {
                #ok(Trie.toArray<Nat, Entry, (Nat, Entry)>(journal, func(id, entry) = (id, entry)));
            };
        };
    };

    public shared (msg) func writeEntry(title : Text, body : Text) : async Result<Text, Text> {
        let journal = Trie.get(journals, journalKey(msg.caller), Principal.equal);
        switch (journal) {
            case (null) { #err("Journal not found") };
            case (?journal) {
                let entry : Entry = {
                    title : Text;
                    body : Text;
                };
                let updatedJournal = Trie.put(journal, entryKey(idCounter), Nat.equal, entry).0;
                journals := Trie.replace(journals, journalKey(msg.caller), Principal.equal, ?updatedJournal).0;
                idCounter += 1;
                #ok("Added entry in journal for " # Principal.toText(msg.caller));
            };
        };
    };

    public shared (msg) func editEntry(id : Nat, title : Text, body : Text) : async Result<Text, Text> {
        let journal = Trie.get(journals, journalKey(msg.caller), Principal.equal);
        switch (journal) {
            case (null) { #err("Journal not found") };
            case (?journal) {
                if (Trie.get(journal, entryKey(id), Nat.equal) == null) {
                    return #err("Entry not found");
                };
                let entry : Entry = {
                    title : Text;
                    body : Text;
                };
                let updatedJournal = Trie.replace(journal, entryKey(id), Nat.equal, ?entry).0;
                journals := Trie.replace(journals, journalKey(msg.caller), Principal.equal, ?updatedJournal).0;
                #ok("Updated entry in journal for " # Principal.toText(msg.caller));
            };
        };
    };

    public shared (msg) func deleteEntry(id : Nat) : async Result<Text, Text> {
        let journal = Trie.get(journals, journalKey(msg.caller), Principal.equal);
        switch (journal) {
            case (null) { #err("Journal not found") };
            case (?journal) {
                if (Trie.get(journal, entryKey(id), Nat.equal) == null) {
                    return #err("Entry not found");
                };
                let updatedJournal = Trie.remove(journal, entryKey(id), Nat.equal).0;
                journals := Trie.replace(journals, journalKey(msg.caller), Principal.equal, ?updatedJournal).0;
                #ok("Removed entry in journal for " # Principal.toText(msg.caller));
            };
        };
    };
};