import Principal "mo:base/Principal";
import Set "mo:base/OrderedSet";
import Map "mo:base/OrderedMap";
import Nat "mo:base/Nat";
import Error "mo:base/Error";
import Result "mo:base/Result";

persistent actor {
    type ShoppingListId = Nat;
    type ItemId = Nat;

    /// An item in a shopping list.
    type Item = {
        name : Text;
        quantity : Nat;
        checked : Bool;
    };

    /// A shopping list.
    type ShoppingList = {
        owners : Set.Set<Principal>;
        items : Map.Map<ItemId, Item>;
    };

    /// Internal errors that can occur.
    type ShoppingListError = {
        #notFound;
        #noAccess;
    };

    private transient let principalSet = Set.Make<Principal>(Principal.compare);
    private transient let shoppingListMap = Map.Make<ShoppingListId>(Nat.compare);
    private transient let itemMap = Map.Make<ItemId>(Nat.compare);
    private var shoppingLists : Map.Map<ShoppingListId, ShoppingList> = shoppingListMap.empty();
    private var nextShoppingListId : ShoppingListId = 0;
    private var nextItemId : ItemId = 0;

    /// Create a new empty shopping list with the caller as the only owner.
    public shared (msg) func createShoppingList() : async ShoppingListId {
        let shoppingListId = nextShoppingListId;
        nextShoppingListId += 1;

        let shoppingList = {
            owners = principalSet.put(principalSet.empty(), msg.caller);
            items = itemMap.empty();
        };
        shoppingLists := shoppingListMap.put(shoppingLists, shoppingListId, shoppingList);

        shoppingListId;
    };

    /// Add a new item to the shopping list. Returns the ID of the new item.
    public shared (msg) func addItem(shoppingListId : ShoppingListId, name : Text, quantity : Nat) : async ItemId {
        let shoppingList = switch (getListAndCheckAccess(shoppingListId, msg.caller)) {
            case (#ok(shoppingList)) { shoppingList };
            case (#err(#notFound)) {
                throw Error.reject("Shopping list not found");
            };
            case (#err(#noAccess)) {
                throw Error.reject("Not an owner of the shopping list");
            };
        };

        let item = {
            name;
            quantity;
            checked = false;
        };

        let itemId = nextItemId;
        let (updatedShoppingLists, _) = shoppingListMap.replace<ShoppingList>(shoppingLists, shoppingListId, { shoppingList with items = shoppingListMap.put(shoppingList.items, itemId, item) }) else throw Error.reject("Shopping list not found");
        nextItemId += 1;
        shoppingLists := updatedShoppingLists;
        itemId;
    };

    /// Remove an item from the shopping list.
    public shared (msg) func removeItem(shoppingListId : ShoppingListId, itemId : ItemId) : async () {
        let shoppingList = switch (getListAndCheckAccess(shoppingListId, msg.caller)) {
            case (#ok(shoppingList)) { shoppingList };
            case (#err(#notFound)) {
                throw Error.reject("Shopping list not found");
            };
            case (#err(#noAccess)) {
                throw Error.reject("Not an owner of the shopping list");
            };
        };

        let (remaining, _) = shoppingListMap.remove(shoppingList.items, itemId) else throw Error.reject("Item not found");
        let (updatedShoppingLists, _) = shoppingListMap.replace<ShoppingList>(shoppingLists, shoppingListId, { shoppingList with items = remaining }) else throw Error.reject("Shopping list not found");
        shoppingLists := updatedShoppingLists;
    };

    /// Check an item in the shopping list.
    public shared (msg) func checkItem(shoppingListId : ShoppingListId, itemId : ItemId) : async () {
        let shoppingList = switch (getListAndCheckAccess(shoppingListId, msg.caller)) {
            case (#ok(shoppingList)) { shoppingList };
            case (#err(#notFound)) {
                throw Error.reject("Shopping list not found");
            };
            case (#err(#noAccess)) {
                throw Error.reject("Not an owner of the shopping list");
            };
        };

        let ?item = shoppingListMap.get(shoppingList.items, itemId) else throw Error.reject("Item not found");
        let updatedItems = shoppingListMap.put(shoppingList.items, itemId, { item with checked = true });
        let (updatedShoppingLists, _) = shoppingListMap.replace<ShoppingList>(shoppingLists, shoppingListId, { shoppingList with items = updatedItems }) else throw Error.reject("Shopping list not found");
        shoppingLists := updatedShoppingLists;
    };

    /// Uncheck an item in the shopping list.
    public shared (msg) func uncheckItem(shoppingListId : ShoppingListId, itemId : ItemId) : async () {
        let shoppingList = switch (getListAndCheckAccess(shoppingListId, msg.caller)) {
            case (#ok(shoppingList)) { shoppingList };
            case (#err(#notFound)) {
                throw Error.reject("Shopping list not found");
            };
            case (#err(#noAccess)) {
                throw Error.reject("Not an owner of the shopping list");
            };
        };

        let ?item = shoppingListMap.get(shoppingList.items, itemId) else throw Error.reject("Item not found");
        let updatedItems = shoppingListMap.put(shoppingList.items, itemId, { item with checked = false });
        let (updatedShoppingLists, _) = shoppingListMap.replace<ShoppingList>(shoppingLists, shoppingListId, { shoppingList with items = updatedItems }) else throw Error.reject("Shopping list not found");
        shoppingLists := updatedShoppingLists;
    };

    /// Get the shopping list.
    public query (msg) func getShoppingList(shoppingListId : ShoppingListId) : async ShoppingList {
        let shoppingList = switch (getListAndCheckAccess(shoppingListId, msg.caller)) {
            case (#ok(shoppingList)) { shoppingList };
            case (#err(#notFound)) {
                throw Error.reject("Shopping list not found");
            };
            case (#err(#noAccess)) {
                throw Error.reject("Not an owner of the shopping list");
            };
        };
        shoppingList;
    };

    /// Get only the unchecked items in the shopping list. Returns a map of item IDs to items.
    public query (msg) func getUncheckedItems(shoppingListId : ShoppingListId) : async Map.Map<ItemId, Item> {
        let shoppingList = switch (getListAndCheckAccess(shoppingListId, msg.caller)) {
            case (#ok(shoppingList)) { shoppingList };
            case (#err(#notFound)) {
                throw Error.reject("Shopping list not found");
            };
            case (#err(#noAccess)) {
                throw Error.reject("Not an owner of the shopping list");
            };
        };
        func filterUnchecked(_ : ItemId, val : Item) : ?Item {
            if (not val.checked) { ?val } else { null };
        };
        let items : Map.Map<ItemId, Item> = shoppingList.items;
        itemMap.mapFilter(items, filterUnchecked);
    };

    /// Share the shopping list with another user.
    public shared (msg) func shareShoppingList(shoppingListId : ShoppingListId, principal : Principal) : async () {
        let shoppingList = switch (getListAndCheckAccess(shoppingListId, msg.caller)) {
            case (#ok(shoppingList)) { shoppingList };
            case (#err(#notFound)) {
                throw Error.reject("Shopping list not found");
            };
            case (#err(#noAccess)) {
                throw Error.reject("Not an owner of the shopping list");
            };
        };

        let updatedOwners = principalSet.put(shoppingList.owners, principal);
        let (updatedShoppingLists, _) = shoppingListMap.replace<ShoppingList>(shoppingLists, shoppingListId, { shoppingList with owners = updatedOwners }) else throw Error.reject("Shopping list not found");
        shoppingLists := updatedShoppingLists;
    };

    /// Delete the shopping list.
    public shared (msg) func deleteShoppingList(shoppingListId : ShoppingListId) : async () {
        let _ = switch (getListAndCheckAccess(shoppingListId, msg.caller)) {
            case (#ok(shoppingList)) { shoppingList };
            case (#err(#notFound)) {
                throw Error.reject("Shopping list not found");
            };
            case (#err(#noAccess)) {
                throw Error.reject("Not an owner of the shopping list");
            };
        };

        let (remaining, _) = shoppingListMap.remove(shoppingLists, shoppingListId) else throw Error.reject("Shopping list not found");
        shoppingLists := remaining;
    };

    /// Get the shopping list and check if the caller has access to it.
    private func getListAndCheckAccess(shoppingListId : ShoppingListId, caller : Principal) : Result.Result<ShoppingList, ShoppingListError> {
        let ?shoppingList = shoppingListMap.get(shoppingLists, shoppingListId) else return #err(#notFound);
        let access = principalSet.contains(shoppingList.owners, caller);
        if (access) { #ok(shoppingList) } else { #err(#noAccess) };
    };
};