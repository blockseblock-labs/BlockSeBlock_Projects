import Result "mo:base/Result";
import Trie "mo:base/Trie";
import Array "mo:base/Array";
import Text "mo:base/Text";
import Principal "mo:base/Principal";
import Option "mo:base/Option";
import Char "mo:base/Char";
import Nat8 "mo:base/Nat8";

persistent actor TicTacToe {
    /// The total number of all games played in the dapp.
    private var totalGamesPlayed : Nat = 0;

    type PlayerStatistics = {
        /// The total number of games won by player X.
        wonGames : Nat;
        /// The total number of games lost by player X.
        lostGames : Nat;
        /// The total number of games tied by player X.
        tiedGames : Nat;
    };

    type GameState = {
        /// board[x][y] can be referenced as board[x * 3 + y].
        board : [Char];
        /// Creator plays as X, the other player as O.
        creator : Principal;
        /// Other player
        otherPlayer : Principal;
        /// The result of the game, recorded from the move finishing the game.
        gameFinishResult : ?(Principal, MoveResult);
    };

    /// The result of a move finishing the game. "Tied" in the current
    /// implementation means that the board is full and no one has won.
    type MoveResult = {
        #won;
        #lost;
        #tied;
    };

    /// Creates a new game state with an empty board.
    private func newGame(creator : Principal, otherPlayer : Principal) : GameState {
        {
            board = [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '];
            creator;
            otherPlayer;
            gameFinishResult = null;
        };
    };

    /// The total number of all games won by player X in the dapp.
    private var principalToGameStatistics = Trie.empty<Principal, PlayerStatistics>();

    /// Maps game IDs to game states. Game IDs are created by the `gameId` function.
    private var pairToGameState = Trie.empty<Text, GameState>();

    /// Creates a new game and overwrites the old game state if it exists.
    //Increments the total number of games played.
    public shared (msg) func createGame(otherPlayer : Principal) {
        let id = gameId(msg.caller, otherPlayer);

        pairToGameState := Trie.put<Text, GameState>(pairToGameState, key(id), Text.equal, newGame(msg.caller, otherPlayer)).0;
        totalGamesPlayed += 1;
    };

    /// Deletes a game and returns the board if the game existed.
    public shared (msg) func deleteGame(otherPlayer : Principal) : async ?[Char] {
        let id = gameId(msg.caller, otherPlayer);

        let (trie, gameState) = Trie.remove<Text, GameState>(pairToGameState, key(id), Text.equal);
        pairToGameState := trie;
        Option.map<GameState, [Char]>(gameState, func(gs : GameState) = gs.board);
    };

    /// Makes a move in the game with `otherPlayer` and returns the result of
    /// the move if this move finished the game. The creator of the game plays
    /// as X. The left bottom corner is (0, 0) and the right upper corner is
    /// (2, 2). Returns an error if the move or no game exists with `otherPlayer`.
    public shared (msg) func makeMove(otherPlayer : Principal, xCoordinate : Nat8, yCoordinate : Nat8) : async Result.Result<?MoveResult, Text> {
        if (xCoordinate > 2 or yCoordinate > 2) {
            return #err("Coordinates out of bounds");
        };

        let id = gameId(msg.caller, otherPlayer);
        let optGameState = Trie.get<Text, GameState>(pairToGameState, key(id), Text.equal);

        let gameState = switch optGameState {
            case null { return #err("Game not found") };
            case (?gameState) { gameState };
        };

        switch (gameState.gameFinishResult) {
            case (?result) {
                return #err("The game has already finished with result: " # debug_show (result));
            };
            case (null) {};
        };

        let board = gameState.board;

        let doesXMoveNext = Array.filter<Char>(board, func(c : Char) = c == 'X').size() == Array.filter<Char>(board, func(c : Char) = c == 'O').size();

        if ((doesXMoveNext and msg.caller != gameState.creator) or (not doesXMoveNext and msg.caller != gameState.otherPlayer)) {
            return #err("It's not your turn");
        };

        let mySymbol = if doesXMoveNext { 'X' } else { 'O' };

        let index : Nat = Nat8.toNat(xCoordinate * 3) + Nat8.toNat(yCoordinate);

        if (gameState.board[index] != ' ') {
            return #err("Field is already occupied");
        };

        let newBoard = Array.mapEntries<Char, Char>(board, func(c, i) = if (i == index) { mySymbol } else { c });

        let moveResult = if (didSomeoneWin(newBoard) != null) {
            incrementStatistics(msg.caller, #won);
            incrementStatistics(otherPlayer, #lost);
            ?#won;
        } else if (isGameTied(newBoard)) {
            incrementStatistics(msg.caller, #tied);
            incrementStatistics(otherPlayer, #tied);
            ?#tied;
        } else { null };

        let gameFinishResult = Option.map<MoveResult, (Principal, MoveResult)>(moveResult, func(mr : MoveResult) = (msg.caller, mr));

        let newGameState = {
            board = newBoard;
            creator = gameState.creator;
            otherPlayer = gameState.otherPlayer;
            gameFinishResult;
        };

        pairToGameState := Trie.put<Text, GameState>(pairToGameState, key(id), Text.equal, newGameState).0;

        #ok moveResult;
    };

    /// Returns the board of the game with `otherPlayer` if it exists.
    public query (msg) func getBoard(otherPlayer : Principal) : async ?[Char] {
        let id = gameId(msg.caller, otherPlayer);
        let optGameState = Trie.get<Text, GameState>(pairToGameState, key(id), Text.equal);
        Option.map<GameState, [Char]>(optGameState, func(gs : GameState) = gs.board);
    };

    /// Returns the board of the game with `otherPlayer` if it exists in a human-readable format.
    public query (msg) func getBoardHumanReadable(otherPlayer : Principal) : async ?Text {
        let id = gameId(msg.caller, otherPlayer);
        let optGameState = Trie.get<Text, GameState>(pairToGameState, key(id), Text.equal);
        Option.map<GameState, Text>(
            optGameState,
            func(gs : GameState) {
                "\n[" # Char.toText(gs.board[2]) # "] [" # Char.toText(gs.board[5]) # "] [" # Char.toText(gs.board[8]) # "]\n" #
                "[" # Char.toText(gs.board[1]) # "] [" # Char.toText(gs.board[4]) # "] [" # Char.toText(gs.board[7]) # "]\n" #
                "[" # Char.toText(gs.board[0]) # "] [" # Char.toText(gs.board[3]) # "] [" # Char.toText(gs.board[6]) # "]\n";
            },
        );
    };

    /// Shows the IDs of all active games for the caller.
    public query (msg) func getMyGameIds() : async Text {
        let callerText = Principal.toText(msg.caller);
        let myGameIds = Trie.mapFilter<Text, GameState, Text>(pairToGameState, func(k : Text, _v : GameState) { if (Text.contains(k, #text callerText)) { ?k } else { null } });
        let myGameIdsArray = Trie.toArray<Text, Text, Text>(myGameIds, func(k : Text, _v : Text) = k);

        "My (" # Principal.toText(msg.caller) # ") game IDs: " # debug_show (myGameIdsArray);
    };

    /// Returns the game statistics of the caller.
    public query (msg) func getMyGameStatistics() : async ?PlayerStatistics {
        let key = { hash = Principal.hash(msg.caller); key = msg.caller };
        Trie.get<Principal, PlayerStatistics>(principalToGameStatistics, key, Principal.equal);
    };

    /// Returns the total number of played/started games by all players.
    public query func getTotalGamesPlayed() : async Nat {
        totalGamesPlayed;
    };

    /// Creates a game ID from two principals. This function is commutative,
    /// i.e., gameId(p1, p2) == gameId(p2, p1).
    private func gameId(p1 : Principal, p2 : Principal) : Text {
        let sorted = (Array.sort([Principal.toText(p1), Principal.toText(p2)], Text.compare));
        Text.join(" & ", sorted.vals());
    };

    /// Returns the winner symbol of the game if there is one, otherwise returns null.
    private func didSomeoneWin(board : [Char]) : ?Char {
        let winPatterns = [
            // rows
            [0, 1, 2],
            [3, 4, 5],
            [6, 7, 8],
            // columns
            [0, 3, 6],
            [1, 4, 7],
            [2, 5, 8],
            // diagonals
            [0, 4, 8],
            [2, 4, 6],
        ];

        for (winPattern in winPatterns.vals()) {
            let (a, b, c) = (winPattern[0], winPattern[1], winPattern[2]);
            if (board[a] != ' ' and board[a] == board[b] and board[b] == board[c]) {
                return ?board[a];
            };
        };

        null;
    };

    private func isBoardFull(board : [Char]) : Bool {
        Array.indexOf<Char>(' ', board, Char.equal) == null;
    };

    private func isGameTied(board : [Char]) : Bool {
        (isBoardFull(board) and didSomeoneWin(board) == null);
    };

    private func key(text : Text) : Trie.Key<Text> {
        { hash = Text.hash(text); key = text };
    };

    /// Increments the statistics of a player. Creates a new entry if statistics
    /// don't yet exist for the player.
    private func incrementStatistics(
        principal : Principal,
        statisticType : MoveResult,
    ) {
        let key = { hash = Principal.hash(principal); key = principal };
        let statistic = Trie.get<Principal, PlayerStatistics>(principalToGameStatistics, key, Principal.equal);
        var result = Option.get(
            statistic,
            { wonGames = 0; lostGames = 0; tiedGames = 0 },
        );
        let increment = switch statisticType {
            case (#won) { (1, 0, 0) };
            case (#lost) { (0, 1, 0) };
            case (#tied) { (0, 0, 1) };
        };
        principalToGameStatistics := Trie.put(
            principalToGameStatistics,
            key,
            Principal.equal,
            {
                wonGames = result.wonGames + increment.0;
                lostGames = result.lostGames + increment.1;
                tiedGames = result.tiedGames + increment.2;
            },
        ).0;
    };
};