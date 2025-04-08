import Error "mo:base/Error";
import List "mo:base/List";
import OrderedMap "mo:base/OrderedMap";
import Text "mo:base/Text";

persistent actor {
    type ActionType = {
        #submitContent;
        #helpOthers;
    };

    type Action = {
        actionType : ActionType;
        points : Nat;
    };

    type User = {
        actions : List.List<Action>;
        reputation : Nat;
    };

    private transient let textMap = OrderedMap.Make<Text>(Text.compare);

    private var users : OrderedMap.Map<Text, User> = textMap.empty<User>();

    public shared func reset() : async () {
        users := textMap.empty<User>();
    };

    public shared func performAction(userId : Text, action : Action) : async () {
        switch (textMap.get(users, userId)) {
            case null {
                let newUser = {
                    actions = List.fromArray<Action>([action]);
                    reputation = action.points;
                };
                users := textMap.put(users, userId, newUser);
            };
            case (?user) {
                let oldActions = user.actions;
                let oldReputation = user.reputation;
                let newUser = {
                    actions = List.push(action, oldActions);
                    reputation = oldReputation + action.points;
                };
                users := textMap.put(users, userId, newUser);
            };
        };
    };

    public shared func getReputation(userId : Text) : async Nat {
        switch (textMap.get(users, userId)) {
            case (?{ reputation }) { reputation };
            case null { throw Error.reject("User not found") };
        };
    };
};