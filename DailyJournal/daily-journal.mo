import Trie "mo:base/Trie";
import Text "mo:base/Text";
import Option "mo:base/Option";
import Array "mo:base/Array";
import Principal "mo:base/Principal";
import Time "mo:base/Time";

persistent actor DailyJournal {

    type Entry = {
        date : Time.Time;
        content : Text;
        tags : [Text];
        reminder : ?Text;
    };

    private func key(user : Principal) : Trie.Key<Principal> {
        { key = user; hash = Principal.hash(user) };
    };

    private var journalEntries : Trie.Trie<Principal, [Entry]> = Trie.empty();

    public shared (msg) func createEntry(date : Time.Time, content : Text, tags : [Text], reminder : ?Text) : async Bool {
        let caller = msg.caller;
        let newEntry : Entry = { date; content; tags; reminder };
        let existingEntries = Option.get(Trie.get(journalEntries, key(caller), Principal.equal), []);

        if (Array.find<Entry>(existingEntries, func(entry) { entry.date == date }) != null) {
            return false;
        };

        let finalEntries = Array.append(existingEntries, [newEntry]);
        journalEntries := Trie.put(journalEntries, key(caller), Principal.equal, finalEntries).0;
        return true;
    };

    public shared (msg) func editEntry(date : Time.Time, newContent : Text) : async Bool {
        let caller = msg.caller;
        switch (Trie.get(journalEntries, key(caller), Principal.equal)) {
            case null { return false };
            case (?entries) {
                var modified = false;
                let updatedEntries = Array.map<Entry, Entry>(
                    entries,
                    func(entry) {
                        if (entry.date == date) {
                            modified := true;
                            {
                                date = entry.date;
                                content = newContent;
                                tags = entry.tags;
                                reminder = entry.reminder;
                            };
                        } else {
                            entry;
                        };
                    },
                );

                if (modified) {
                    journalEntries := Trie.put(journalEntries, key(caller), Principal.equal, updatedEntries).0;
                    return true;
                } else {
                    return false;
                };
            };
        };
    };

    public shared (msg) func deleteEntry(date : Time.Time) : async Bool {
        let caller = msg.caller;
        switch (Trie.get(journalEntries, key(caller), Principal.equal)) {
            case null { return false };
            case (?entries) {
                let filteredEntries = Array.filter<Entry>(entries, func(entry) { entry.date != date });
                if (Array.size(filteredEntries) == Array.size(entries)) {
                    return false;
                };
                if (Array.size(filteredEntries) == 0) {
                    journalEntries := Trie.remove(journalEntries, key(caller), Principal.equal).0;
                } else {
                    journalEntries := Trie.put(journalEntries, key(caller), Principal.equal, filteredEntries).0;
                };
                return true;
            };
        };
    };

    public shared (msg) func getEntries() : async [Entry] {
        let caller = msg.caller;
        return Option.get(Trie.get(journalEntries, key(caller), Principal.equal), []);
    };

    public shared (msg) func getEntriesByTag(tag : Text) : async [Entry] {
        let caller = msg.caller;
        let entries = Option.get(Trie.get(journalEntries, key(caller), Principal.equal), []);
        return Array.filter<Entry>(entries, func(entry) { Array.find<Text>(entry.tags, func(t) { t == tag }) != null });
    };
};