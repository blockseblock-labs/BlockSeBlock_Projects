import Text "mo:base/Text";
import Trie "mo:base/Trie";
import Nat "mo:base/Nat";
import Hash "mo:base/Hash";
import Result "mo:base/Result";
import Principal "mo:base/Principal";

persistent actor QnAPlatform {
    type Answer = {
        id : Nat;
        content : Text;
        responder : Principal;
    };

    type Question = {
        id : Nat;
        content : Text;
        asker : Principal;
        answers : Trie.Trie<Nat, Answer>;
    };

    type Questions = Trie.Trie<Nat, Question>;
    type Result<T, E> = Result.Result<T, E>;

    private var questionIdCounter : Nat = 0;
    private var answerIdCounter : Nat = 0;

    private var questions : Questions = Trie.empty();

    func customHash(n : Nat) : Hash.Hash {
        Text.hash(Nat.toText(n));
    };

    func entryKey(id : Nat) : Trie.Key<Nat> {
        { hash = customHash(id); key = id };
    };

    public shared (msg) func postQuestion(content : Text) : async Result<Text, Text> {
        let newQuestion : Question = {
            id = questionIdCounter;
            content = content;
            asker = msg.caller;
            answers = Trie.empty();
        };

        questionIdCounter += 1;

        questions := Trie.put(questions, entryKey(newQuestion.id), Nat.equal, newQuestion).0;

        #ok "Question posted successfully";
    };

    public shared (msg) func postAnswer(questionId : Nat, answerContent : Text) : async Result<Text, Text> {
        let question = Trie.get(questions, entryKey(questionId), Nat.equal);

        switch (question) {
            case null { #err("Question not found") };
            case (?qst) {
                let newAnswer : Answer = {
                    id = answerIdCounter;
                    content = answerContent;
                    responder = msg.caller;
                };

                answerIdCounter += 1;

                let updatedAnswers = Trie.put(qst.answers, entryKey(newAnswer.id), Nat.equal, newAnswer).0;

                let updatedQuestion : Question = {
                    id = qst.id;
                    content = qst.content;
                    asker = qst.asker;
                    answers = updatedAnswers;
                };

                questions := Trie.put(questions, entryKey(questionId), Nat.equal, updatedQuestion).0;

                #ok "Answer posted successfully";
            };
        };
    };

    public query func viewAllQuestions() : async Result<[(Nat, Question)], Text> {
        #ok(Trie.toArray<Nat, Question, (Nat, Question)>(questions, func(id, question) = (id, question)));
    };

    public query func viewQuestionById(questionId : Nat) : async Result<Question, Text> {
        let question = Trie.get(questions, entryKey(questionId), Nat.equal);
        switch (question) {
            case null {
                #err "Question not found";
            };
            case (?qst) {
                #ok qst;
            };
        };
    };
};