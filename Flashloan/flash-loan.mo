import Nat "mo:base/Nat";
import Principal "mo:base/Principal";
import Float "mo:base/Float";
import Time "mo:base/Time";
import Int "mo:base/Int";
import Map "mo:base/OrderedMap";

persistent actor FlashLoanProtocol {
    var liquidityPool : Nat = 1_000_000;
    var feeRate : Float = 0.001;
    let nanosecondsPerDay : Nat = 86_400_000_000_000; // 86,400 seconds * 1_000_000_000 nanoseconds
    transient let loanMap = Map.Make<Principal>(Principal.compare);
    var loans : Map.Map<Principal, Loan> = loanMap.empty<Loan>();
    type Loan = {
        principal : Nat;
        repaymentAmount : Nat;
        dueTime : Int;
        active : Bool;
    };
    public shared ({ caller }) func flashLoan(amount : Nat, durationDays : Nat) : async ?Nat {
        if (loanMap.get(loans, caller) != null) return null;
        if (amount > liquidityPool) return null;
        liquidityPool -= amount;
        let interest = Int.abs(Float.toInt(Float.fromInt(amount) * feeRate * Float.fromInt(durationDays)));
        let repaymentAmount = amount + interest;
        let dueTime = Time.now() + (durationDays * nanosecondsPerDay);
        loans := loanMap.put(loans, caller, { principal = amount; repaymentAmount = repaymentAmount; dueTime = dueTime; active = true });
        ?repaymentAmount;
    };
    public shared ({ caller }) func repayLoan() : async Bool {
        let ?loan = loanMap.get(loans, caller) else return false;
        if (Time.now() >= loan.dueTime) return false;
        liquidityPool += loan.repaymentAmount;
        let (_loans, _old) = loanMap.remove(loans, caller);
        loans := _loans;
        true;
    };
    public query func getLiquidity() : async Nat {
        liquidityPool;
    };
    public query (msg) func getLoanDetails() : async ?Loan {
        loanMap.get(loans, msg.caller);
    };
};