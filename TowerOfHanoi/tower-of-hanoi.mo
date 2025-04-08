import Array "mo:base/Array";
import Nat "mo:base/Nat";

persistent actor TowerOfHanoi {

    public func solveHanoi(n : Nat, source : Text, auxiliary : Text, target : Text) : async ([Text], Nat) {
        var steps : [Text] = [];
        var stepCount : Nat = 0;

        func hanoi(n : Nat, source : Text, auxiliary : Text, target : Text) {
            if (n == 1) {
                steps := Array.append(steps, ["Move disk 1 from " # source # " to " # target]);
                stepCount += 1;
            } else {
                hanoi(n - 1, source, target, auxiliary);
                steps := Array.append(steps, ["Move disk " # Nat.toText(n) # " from " # source # " to " # target]);
                stepCount += 1;
                hanoi(n - 1, auxiliary, source, target);
            };
        };

        hanoi(n, source, auxiliary, target);
        return (steps, stepCount);
    };
};