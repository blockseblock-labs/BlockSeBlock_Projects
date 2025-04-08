import Array "mo:base/Array";
import Random "mo:base/Random";
import Nat8 "mo:base/Nat8";

persistent actor Blackjack {
    private var deck : [var Card] = [var];
    private var topCardIndex = 0;
    private var gameEnded = true;

    private var playerHand : [Card] = [];
    private var dealerHand : [Card] = [];

    type Card = {
        suit : Text;
        rank : Text;
        value : Nat;
    };

    type Status = {
        #ongoing;
        #ended;
    };

    type GameState = {
        playerScore : Nat;
        dealerScore : Nat;
        gameStatus : Status;
        playerHand : [Card];
        dealerHand : [Card];
    };

    private let suits = ["Hearts", "Diamonds", "Clubs", "Spades"];

    private let ranks = [
        { rank = "2"; value = 2 },
        { rank = "3"; value = 3 },
        { rank = "4"; value = 4 },
        { rank = "5"; value = 5 },
        { rank = "6"; value = 6 },
        { rank = "7"; value = 7 },
        { rank = "8"; value = 8 },
        { rank = "9"; value = 9 },
        { rank = "10"; value = 10 },
        { rank = "Jack"; value = 10 },
        { rank = "Queen"; value = 10 },
        { rank = "King"; value = 10 },
        { rank = "Ace"; value = 11 },
    ];

    public func reset() : async Text {
        playerHand := [];
        dealerHand := [];
        gameEnded := true;
        "The state has been reset!";
    };

    public func initGame() : async Text {
        createDeck();
        await shuffleDeck();
        playerHand := [drawCard(), drawCard()];
        dealerHand := [drawCard()];
        gameEnded := false;
        "The game has been initialized!";
    };

    private func createDeck() {
        let dummyCard = { suit = ""; rank = ""; value = 0 };
        deck := Array.init(suits.size() * ranks.size(), dummyCard);
        var index = 0;
        for (suit in suits.vals()) {
            for (rank in ranks.vals()) {
                deck[index] := { rank with suit };
                index += 1;
            };
        };
    };

    private func shuffleDeck() : async () {
        var index = 0;
        var random = Random.Finite "";
        while (index + 1 < deck.size()) {
            switch (random.byte()) {
                case (?num) {
                    let randomIndex = (Nat8.toNat(num) % (deck.size() - index) : Nat) + index;
                    let tmp : Card = deck[index];
                    deck[index] := deck[randomIndex];
                    deck[randomIndex] := tmp;
                    index += 1;
                };
                case null {
                    random := Random.Finite(await Random.blob());
                };
            };
        };
    };

    private func drawCard() : Card {
        topCardIndex += 1;
        return deck[topCardIndex - 1];
    };

    public func getGameState() : async GameState {
        let gameStatus = if gameEnded #ended else #ongoing;
        {
            playerScore = calculateHandValue(playerHand);
            dealerScore = calculateHandValue(dealerHand);
            gameStatus;
            playerHand;
            dealerHand;
        };
    };

    public func hit() : async Text {
        if gameEnded {
            "The game is over.";
        } else {
            playerHand := Array.append(playerHand, [drawCard()]);
            if (calculateHandValue(playerHand) > 21) {
                gameEnded := true;
                "You lost!";
            } else {
                "Card added to your hand.";
            };
        };

    };

    public func stand() : async Text {
        gameEnded := true;
        while (calculateHandValue(dealerHand) < 17) {
            dealerHand := Array.append(dealerHand, [drawCard()]);
        };

        let playerScore = calculateHandValue(playerHand);
        let dealerScore = calculateHandValue(dealerHand);

        if (dealerScore > 21 or playerScore > dealerScore) {
            "You win!";
        } else if (dealerScore == playerScore) {
            "It's a tie!";
        } else {
            "The dealer wins!";
        };
    };

    private func calculateHandValue(hand : [Card]) : Nat {
        let total = Array.foldLeft<Card, Nat>(
            hand,
            0,
            func(acc, card) {
                acc + card.value;
            },
        );

        let numAces = Array.foldLeft<Card, Nat>(
            hand,
            0,
            func(acc, card) {
                acc + (if (card.rank == "Ace") 1 else 0);
            },
        );

        var adjustedTotal = total;
        var remainingAces = numAces;

        while (adjustedTotal > 21 and remainingAces > 0) {
            adjustedTotal := adjustedTotal - 10;
            remainingAces := remainingAces - 1;
        };

        adjustedTotal;
    };
};