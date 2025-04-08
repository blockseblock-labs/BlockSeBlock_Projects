import Nat8 "mo:base/Nat8";
import OrderedMap "mo:base/OrderedMap";
import OrderedSet "mo:base/OrderedSet";
import Random "mo:base/Random";
import Nat "mo:base/Nat";
import Principal "mo:base/Principal";
import Iter "mo:base/Iter";
import Time "mo:base/Time";
import Result "mo:base/Result";
import Int "mo:base/Int";

persistent actor class Lottery() {
    // Types
    type TicketId = Nat;
    type Participant = Principal;
    type Prize = Nat;

    // Custom errors
    type LotteryError = {
        #InsufficientFunds;
        #InvalidTicket;
        #LotteryNotOpen;
        #UnauthorizedAccess;
        #NoParticipants;
        #LotteryInProgress;
    };

    // State (reinitialized from stable)
    private transient let participantOps = OrderedSet.Make<Participant>(Principal.compare);
    private var participants : OrderedSet.Set<Participant> = participantOps.empty();
    private transient let ticketOps = OrderedMap.Make<TicketId>(Nat.compare);
    private var tickets : OrderedMap.Map<TicketId, Participant> = ticketOps.empty();
    private var nextTicketId : TicketId = 0;
    private var prizePool : Prize = 0;
    private var isLotteryOpen : Bool = true;
    private var roundStartTime : Time.Time = Time.now();
    private var minParticipants : Nat = 3;

    // Constants
    private let TICKET_PRICE : Prize = 100;
    private let maxRoundDuration : Nat = 86_400_000_000_000; // 24 hours in nanoseconds
    private transient var randomGen : Random.Finite = Random.Finite("");

    // Buy a ticket with automatic draw checks
    public shared (msg) func buyTicket() : async Result.Result<TicketId, LotteryError> {
        if (not isLotteryOpen) {
            return #err(#LotteryNotOpen);
        };

        let participant = msg.caller;

        // Add participant and ticket
        let ticketId = nextTicketId;
        nextTicketId += 1;
        participants := participantOps.put(participants, participant);
        tickets := ticketOps.put(tickets, ticketId, participant);
        prizePool += TICKET_PRICE;

        // Check draw conditions after adding new participant
        let currentTime = Time.now();
        let participantCount = participantOps.size(participants);
        let timeExpired = currentTime > roundStartTime + maxRoundDuration;

        if (participantCount >= minParticipants or timeExpired) {
            await tryAutoDraw();
        };

        #ok(ticketId);
    };

    // Automatic draw mechanism
    private func tryAutoDraw() : async () {
        if (not isLotteryOpen) return;

        isLotteryOpen := false;
        let ticketCount = ticketOps.size(tickets);

        if (ticketCount > 0) {
            let randomIndex = await generateRandomIndex(ticketCount);
            let ticketEntries = Iter.toArray(ticketOps.entries(tickets));
            let (_winningTicket, _winner) = ticketEntries[randomIndex];

            // Reset lottery state
            prizePool := 0;
            participants := participantOps.empty();
            tickets := ticketOps.empty();
            nextTicketId := 0;
        };

        // Start new round
        roundStartTime := Time.now();
        isLotteryOpen := true;
    };

    // Query functions
    public query func getTicketOwner(ticketId : TicketId) : async Result.Result<Principal, LotteryError> {
        switch (ticketOps.get(tickets, ticketId)) {
            case (null) { #err(#InvalidTicket) };
            case (?owner) { #ok(owner) };
        };
    };

    public query func getParticipantCount() : async Nat {
        participantOps.size(participants);
    };

    public query func getParticipants() : async [Participant] {
        Iter.toArray(participantOps.vals(participants));
    };

    public query func getPrizePool() : async Prize {
        prizePool;
    };

    public query func timeUntilNextDraw() : async Nat {
        let remaining = roundStartTime + maxRoundDuration - Time.now();
        if (remaining > 0) { Int.abs(remaining / 1_000_000_000) } else { 0 };
    };

    // Random generator
    private func generateRandomIndex(max : Nat) : async Nat {
        switch (randomGen.byte()) {
            case (null) {
                randomGen := Random.Finite(await Random.blob());
                let ?randomResult = randomGen.byte() else return 0;
                Nat8.toNat(randomResult) % max;
            };
            case (?randomResult) {
                Nat8.toNat(randomResult) % max;
            };
        };
    };
};