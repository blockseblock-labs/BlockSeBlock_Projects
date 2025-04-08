import Random "mo:base/Random";
import Nat32 "mo:base/Nat32";
import Error "mo:base/Error";

persistent actor {
    /// The number to guess. Null if the game is not started
    private var number : ?Nat32 = null;
    /// The number of attempts it takes to guess the number
    private var attempts : Nat = 0;

    /// The result of a guess
    type GuessResult = {
        result : Bool;
        message : Text;
        attempts : Nat;
    };

    /// Starts a new game with a random number in the given range
    public shared func startGame(rangeStart : Nat32, rangeEnd : Nat32) : async () {
        if (rangeEnd < rangeStart) {
            throw Error.reject("Range end must be greater than or equal to range start");
        };
        let random = Random.Finite(await Random.blob());
        let ?generated_number = random.range(32) else throw Error.reject("not enough entropy?");
        number := ?(Nat32.fromNat(generated_number) % (rangeEnd - rangeStart + 1) + rangeStart);
        attempts := 0;
    };

    /// Try to guess the number and get the guess result.
    public shared func tryGuess(guess : Nat32) : async GuessResult {
        let ?n = number else return {
            result = false;
            message = "Game not started yet";
            attempts = 0;
        };

        attempts := attempts + 1;
        if (guess == n) {
            number := null;
            return {
                result = true;
                message = "You guessed right!";
                attempts;
            };
        };

        {
            result = false;
            message = "You guessed wrong, try again";
            attempts;
        };
    };
};