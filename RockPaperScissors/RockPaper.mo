import Nat "mo:base/Nat";
import Random "mo:base/Random";

persistent actor RockPaperScissors {

    // Possible moves.
    type Move = {
        #rock;
        #paper;
        #scissors;
    };

    // Possible game results from the perspective of the player.
    type Result = {
        #win;
        #lose;
        #tie;
    };

    type Response = {
        result : Result;
        canisterMove : Move;
        numWins : Nat;
        numLosses : Nat;
        numTies : Nat;
    };

    // Number of wins, losses, and ties from the perspective of the player.
    private var numWins : Nat = 0;
    private var numLosses : Nat = 0;
    private var numTies : Nat = 0;

    // The player's previous moves.
    private var previousMoves : (Move, Move) = (#rock, #paper);
    private var previousResults : (Result, Result) = (#tie, #tie);

    func getMove() : async Move {

        let random = Random.Finite(await Random.blob());

        switch (random.range(2)) {
            case (?randomNumber) {
                if (randomNumber == 0) {
                    // Choose randomly with 25% probability.
                    chooseRandomly(random);
                } else {
                    chooseDeterministically();
                };
            };
            case null {
                // If no randomness is available, choose deterministically.
                chooseDeterministically();
            };
        };
    };

    func chooseDeterministically() : Move {
        // If the player chose the same move twice in a row, pick the move to beat it.
        switch (previousMoves) {
            case (#rock, #rock) {
                return #paper;
            };
            case (#paper, #paper) {
                return #scissors;
            };
            case (#scissors, #scissors) {
                return #rock;
            };
            case _ {};
        };

        switch (previousResults.1) {
            // Winners tend to repeat winning moves.
            case (#win) {
                switch (previousMoves.1) {
                    case (#rock) { #paper };
                    case (#paper) { #scissors };
                    case (#scissors) { #rock };
                };
            };
            // Losers tent to switch to the move that beats what the winner played.
            case (#lose) {
                previousMoves.1;
            };
            // If there is a tie, players tend to switch to the move that beats the move that caused the tie.
            case (#tie) {
                switch (previousMoves.1) {
                    case (#rock) { #scissors };
                    case (#paper) { #rock };
                    case (#scissors) { #paper };
                };
            };
        };
    };

    func chooseRandomly(random : Random.Finite) : Move {
        switch (random.range(16)) {
            case (?randomNumber) {
                // "Scissors" is just marginally more likely when using 16 bits.
                if (randomNumber < 21845) {
                    #rock;
                } else if (randomNumber < 43690) {
                    #paper;
                } else {
                    #scissors;
                };
            };
            case null { #rock }; // If there is no randomness, choose "rock".
        };
    };

    // Function to play the game against the canister.
    public func play(playerMove : Move) : async Response {
        let canisterMove = await getMove();
        let result : Result = switch (playerMove) {
            case (#rock) {
                switch (canisterMove) {
                    case (#rock) { #tie };
                    case (#paper) { #lose };
                    case (#scissors) { #win };
                };
            };
            case (#paper) {
                switch (canisterMove) {
                    case (#rock) { #win };
                    case (#paper) { #tie };
                    case (#scissors) { #lose };
                };
            };
            case (#scissors) {
                switch (canisterMove) {
                    case (#rock) { #lose };
                    case (#paper) { #win };
                    case (#scissors) { #tie };
                };
            };
        };
        switch (result) {
            case (#win) { numWins += 1 };
            case (#lose) { numLosses += 1 };
            case (#tie) { numTies += 1 };
        };
        previousMoves := (previousMoves.1, playerMove);
        previousResults := (previousResults.1, result);

        { result; canisterMove; numWins; numLosses; numTies };
    };

    // Endpoint to reset the game
    public func reset() : async () {
        numWins := 0;
        numLosses := 0;
        numTies := 0;
    };
};