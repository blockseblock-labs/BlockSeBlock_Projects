import Trie "mo:base/Trie";
import Result "mo:base/Result";
import Hash "mo:base/Hash";
import Text "mo:base/Text";
import Nat "mo:base/Nat";
import Array "mo:base/Array";
import Order "mo:base/Order";

persistent actor {
    type Flashcard = {
        question : Text;
        answer : Text;
        confidence : Nat;
    };

    type Result<T, E> = Result.Result<T, E>;

    type Key<K> = Trie.Key<K>;

    func customHash(n : Nat) : Hash.Hash {
        Text.hash(Nat.toText(n));
    };

    func key(id : Nat) : Key<Nat> { { hash = customHash id; key = id } };

    private var flashcards : Trie.Trie<Nat, Flashcard> = Trie.empty();
    private var idCounter : Nat = 0;

    public func createFlashcard(question : Text, answer : Text) : async Result<Nat, Text> {
        let id = idCounter;
        idCounter += 1;
        let flashcard : Flashcard = {
            question = question;
            answer = answer;
            confidence = 0;
        };
        flashcards := Trie.put<Nat, Flashcard>(flashcards, key id, Nat.equal, flashcard).0;
        #ok(id);
    };

    public query func readFlashcard(id : Nat) : async Result<Flashcard, Text> {
        let flashcard = Trie.get<Nat, Flashcard>(flashcards, key id, Nat.equal);
        switch (flashcard) {
            case null { #err("Flashcard not found") };
            case (?flashcard) { #ok(flashcard) };
        };
    };

    public func updateFlashcard(id : Nat, question : Text, answer : Text) : async Result<Text, Text> {
        let flashcard = Trie.get<Nat, Flashcard>(flashcards, key id, Nat.equal);
        switch (flashcard) {
            case null { #err("Flashcard not found") };
            case (?flashcard) {
                let updatedFlashcard : Flashcard = {
                    question = question;
                    answer = answer;
                    confidence = flashcard.confidence;
                };
                flashcards := Trie.replace<Nat, Flashcard>(flashcards, key id, Nat.equal, ?updatedFlashcard).0;
                #ok("Flashcard updated successfully");
            };
        };
    };

    public func deleteFlashcard(id : Nat) : async Result<Text, Text> {
        flashcards := Trie.remove<Nat, Flashcard>(flashcards, key id, Nat.equal).0;
        #ok("Flashcard deleted successfully");
    };

    public query func listFlashcards() : async Result<[(Nat, Flashcard)], Text> {
        #ok(Trie.toArray<Nat, Flashcard, (Nat, Flashcard)>(flashcards, func(id, flashcard) = (id, flashcard)));
    };

    public func rateFlashcard(id : Nat, confidence : Nat) : async Result<Text, Text> {
        if (confidence < 1 or confidence > 5) {
            return #err("Confidence must be between 1 and 5");
        };

        let flashcard = Trie.get<Nat, Flashcard>(flashcards, key id, Nat.equal);
        switch (flashcard) {
            case null { #err("Flashcard not found") };
            case (?flashcard) {
                let updatedFlashcard : Flashcard = {
                    question = flashcard.question;
                    answer = flashcard.answer;
                    confidence = confidence;
                };
                flashcards := Trie.replace<Nat, Flashcard>(flashcards, key id, Nat.equal, ?updatedFlashcard).0;
                #ok("Flashcard rated successfully");
            };
        };
    };

    public query func listFlashcardsSortedByConfidence() : async Result<[(Nat, Flashcard)], Text> {
        #ok(
            Array.sort(
                Trie.toArray<Nat, Flashcard, (Nat, Flashcard)>(
                    flashcards,
                    func(id, flashcard) = (id, flashcard),
                ),
                func(a : (Nat, Flashcard), b : (Nat, Flashcard)) : Order.Order {
                    if (a.1.confidence < b.1.confidence) #less else if (a.1.confidence > b.1.confidence) #greater else #equal;
                },
            )
        );
    };
};