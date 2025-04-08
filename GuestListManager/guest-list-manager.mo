import Principal "mo:base/Principal";
import Map "mo:base/OrderedMap";
import Array "mo:base/Array";
import Error "mo:base/Error";

persistent actor {

    type Guest = {
        name : Text;
    };

    type GuestList = {
        owner : Principal;
        guests : [Guest];
    };

    private transient let guestListMap = Map.Make<Principal>(Principal.compare);
    private var guestLists : Map.Map<Principal, GuestList> = guestListMap.empty();

    private func ensureGuestList(caller : Principal) : () {
        switch (guestListMap.get(guestLists, caller)) {
            case null {
                let guestList = {
                    owner = caller;
                    guests = [];
                };
                guestLists := guestListMap.put(guestLists, caller, guestList);
            };
            case _ {};
        };
    };

    public shared ({ caller }) func addGuest(name : Text) : async () {
        ensureGuestList(caller);

        let guestList = switch (guestListMap.get(guestLists, caller)) {
            case (?list) list;
            case null throw Error.reject("Guest list not found for the caller");
        };

        if (Array.find<Guest>(guestList.guests, func(guest) = guest.name == name) != null) {
            throw Error.reject("Guest already exists in the list");
        };

        let updatedGuests = Array.append(guestList.guests, [{ name }]);
        let updatedGuestList = { guestList with guests = updatedGuests };
        guestLists := guestListMap.put(guestLists, caller, updatedGuestList);
    };

    public shared (msg) func deleteGuestList() : async () {
        let caller = msg.caller;
        let (remaining, _) = guestListMap.remove(guestLists, caller) else throw Error.reject("Guest list not found for the caller");
        guestLists := remaining;
    };

    public shared (msg) func deleteGuestByName(name : Text) : async () {
        let caller = msg.caller;
        let guestList = switch (guestListMap.get(guestLists, caller)) {
            case (?list) list;
            case null throw Error.reject("Guest list not found for the caller");
        };

        let updatedGuests = Array.filter(guestList.guests, func(guest : Guest) : Bool = guest.name != name);

        if (Array.size(updatedGuests) == Array.size(guestList.guests)) {
            throw Error.reject("Guest with the given name not found");
        };

        let updatedGuestList = { guestList with guests = updatedGuests };
        guestLists := guestListMap.put(guestLists, caller, updatedGuestList);
    };

    public query (msg) func getGuestList() : async GuestList {
        let caller = msg.caller;
        let guestList = switch (guestListMap.get(guestLists, caller)) {
            case (?list) list;
            case null throw Error.reject("Guest list not found for the caller");
        };
        guestList;
    };
};