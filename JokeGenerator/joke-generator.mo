import Error "mo:base/Error";
import Principal "mo:base/Principal";
import Random "mo:base/Random";

persistent actor {
    private let jokes : [Text] = [
        "Why don't scientists trust atoms? Because they make up everything!",
        "Why did the scarecrow win an award? Because he was outstanding in his field!",
        "Why don't skeletons fight each other? They don't have the guts!",
        "What do you call cheese that isn't yours? Nacho cheese!",
        "Why can't you hear a pterodactyl in the bathroom? Because the 'P' is silent!",
        "What do you call a pile of cats? A meowtain.",
        "Why did the tomato turn red? Because it saw the salad dressing.",
        "What do you call a fake noodle? An impasta.",
        "Why did the golfer bring two pairs of pants? In case he got a hole in one.",
        "What do you call a bear with no teeth? A gummy bear.",
        "Why did the math book look sad? Because it had too many problems.",
        "What do you call a belt made out of watches? A waist of time.",
        "Why did the bicycle fall over? Because it was two tired.",
        "What do you call a cow with no legs? Ground beef.",
        "Why did the coffee file a police report? It got mugged.",
        "What do you call a factory that makes okay products? A satisfactory.",
    ];

    public shared ({ caller }) func generateJoke() : async Text {
        if (Principal.isAnonymous(caller)) {
            throw Error.reject("An anonymous user is not allowed to generate jokes.");
        };

        let index = await generateRandomNat(jokes.size() - 1);
        jokes[index];
    };

    private func generateRandomNat(max : Nat) : async Nat {
        let rng = Random.Finite(await Random.blob());

        switch (rng.range(32)) {
            case null 0;
            case (?value) value % max;
        };
    };
};