import Nat "mo:base/Nat";
import Text "mo:base/Text";
import List "mo:base/List";
import Time "mo:base/Time";
import Trie "mo:base/Trie";
import Principal "mo:base/Principal";
import Result "mo:base/Result";
import Array "mo:base/Array";
import Int "mo:base/Int";

persistent actor HotelBooking {
    // Types
    type BookingId = Nat;
    type RoomId = Nat;
    type CustomerId = Principal;

    type RoomType = {
        #standard;
        #deluxe;
        #suite;
        #presidential;
    };

    type RoomStatus = {
        #available;
        #occupied;
        #maintenance;
    };

    type Room = {
        id : RoomId;
        roomNumber : Text;
        roomType : RoomType;
        pricePerNight : Nat;
        status : RoomStatus;
        capacity : Nat;
    };

    type Booking = {
        id : BookingId;
        customerId : CustomerId;
        roomId : RoomId;
        checkInDate : Time.Time;
        checkOutDate : Time.Time;
        totalPrice : Nat;
        status : BookingStatus;
        guestCount : Nat;
    };

    type BookingStatus = {
        #confirmed;
        #cancelled;
        #completed;
    };

    // State variables
    private var nextBookingId : Nat = 1;
    private var nextRoomId : Nat = 1;

    private var rooms : Trie.Trie<RoomId, Room> = Trie.empty();
    private var bookings : Trie.Trie<BookingId, Booking> = Trie.empty();
    private var customerBookings : Trie.Trie<CustomerId, List.List<BookingId>> = Trie.empty();
    private var availableRooms : Trie.Trie<RoomId, ()> = Trie.empty();

    // Helper functions
    private func key(n : Nat) : Trie.Key<Nat> {
        { key = n; hash = Text.hash(Nat.toText(n)) };
    };

    private func principalKey(p : Principal) : Trie.Key<Principal> {
        { key = p; hash = Text.hash(Principal.toText(p)) };
    };

    private func intToNat(x : Int) : Nat {
        if (x < 0) {
            return 0;
        };
        let absX = Int.abs(x);
        let text = Int.toText(absX);
        switch (Nat.fromText(text)) {
            case (?nat) nat;
            case null 0;
        };
    };

    // Room Management Functions
    public func addRoom(
        roomNumber : Text,
        roomType : RoomType,
        pricePerNight : Nat,
        capacity : Nat,
    ) : async RoomId {
        let room : Room = {
            id = nextRoomId;
            roomNumber = roomNumber;
            roomType = roomType;
            pricePerNight = pricePerNight;
            status = #available;
            capacity = capacity;
        };

        rooms := Trie.put(
            rooms,
            key(nextRoomId),
            Nat.equal,
            room,
        ).0;

        availableRooms := Trie.put(
            availableRooms,
            key(nextRoomId),
            Nat.equal,
            (),
        ).0;

        let currentId = nextRoomId;
        nextRoomId += 1;
        currentId;
    };

    // Booking Functions
    public shared (msg) func makeBooking(
        roomId : RoomId,
        checkInDate : Time.Time,
        checkOutDate : Time.Time,
        guestCount : Nat,
    ) : async Result.Result<BookingId, Text> {
        let customerId = msg.caller;

        if (checkInDate >= checkOutDate) {
            return #err("Invalid date range");
        };

        switch (Trie.get(rooms, key(roomId), Nat.equal)) {
            case null {
                return #err("Room not found");
            };
            case (?room) {
                if (room.status != #available) {
                    return #err("Room is not available");
                };
                if (room.capacity < guestCount) {
                    return #err("Room capacity exceeded");
                };

                let durationNanos = checkOutDate - checkInDate;
                let durationDays = intToNat(durationNanos / (24 * 60 * 60 * 1_000_000_000));
                let totalPrice = room.pricePerNight * durationDays;

                let booking : Booking = {
                    id = nextBookingId;
                    customerId = customerId;
                    roomId = roomId;
                    checkInDate = checkInDate;
                    checkOutDate = checkOutDate;
                    totalPrice = totalPrice;
                    status = #confirmed;
                    guestCount = guestCount;
                };

                bookings := Trie.put(
                    bookings,
                    key(nextBookingId),
                    Nat.equal,
                    booking,
                ).0;

                let existingBookings = switch (Trie.get(customerBookings, principalKey(customerId), Principal.equal)) {
                    case null List.nil<BookingId>();
                    case (?list) list;
                };

                customerBookings := Trie.put(
                    customerBookings,
                    principalKey(customerId),
                    Principal.equal,
                    List.push(nextBookingId, existingBookings),
                ).0;

                let updatedRoom = {
                    room with
                    status = #occupied;
                };

                rooms := Trie.put(
                    rooms,
                    key(roomId),
                    Nat.equal,
                    updatedRoom,
                ).0;

                availableRooms := Trie.remove(
                    availableRooms,
                    key(roomId),
                    Nat.equal,
                ).0;

                let currentId = nextBookingId;
                nextBookingId += 1;
                #ok(currentId);
            };
        };
    };

    public shared (msg) func cancelBooking(bookingId : BookingId) : async Result.Result<(), Text> {
        switch (Trie.get(bookings, key(bookingId), Nat.equal)) {
            case null {
                #err("Booking not found");
            };
            case (?booking) {
                if (booking.customerId != msg.caller) {
                    return #err("Not authorized");
                };

                if (booking.status != #confirmed) {
                    return #err("Booking cannot be cancelled");
                };

                let updatedBooking = {
                    booking with
                    status = #cancelled;
                };

                bookings := Trie.put(
                    bookings,
                    key(bookingId),
                    Nat.equal,
                    updatedBooking,
                ).0;

                switch (Trie.get(rooms, key(booking.roomId), Nat.equal)) {
                    case null {
                        #err("Room not found");
                    };
                    case (?room) {
                        let updatedRoom = {
                            room with
                            status = #available;
                        };

                        rooms := Trie.put(
                            rooms,
                            key(booking.roomId),
                            Nat.equal,
                            updatedRoom,
                        ).0;

                        availableRooms := Trie.put(
                            availableRooms,
                            key(booking.roomId),
                            Nat.equal,
                            (),
                        ).0;

                        #ok(());
                    };
                };
            };
        };
    };

    // Query Functions
    public query func getAvailableRooms() : async [Room] {
        let available = Trie.toArray<RoomId, (), (RoomId, ())>(availableRooms, func(k, v) = (k, v));
        Array.mapFilter<(RoomId, ()), Room>(
            available,
            func((id, _)) {
                Trie.get(rooms, key(id), Nat.equal);
            },
        );
    };

    public query func getRoomDetails(roomId : RoomId) : async Result.Result<Room, Text> {
        switch (Trie.get(rooms, key(roomId), Nat.equal)) {
            case null #err("Room not found");
            case (?room) #ok(room);
        };
    };

    public query func getBookingDetails(bookingId : BookingId) : async Result.Result<Booking, Text> {
        switch (Trie.get(bookings, key(bookingId), Nat.equal)) {
            case null #err("Booking not found");
            case (?booking) #ok(booking);
        };
    };

    public query (msg) func getMyBookings() : async [Booking] {
        switch (Trie.get(customerBookings, principalKey(msg.caller), Principal.equal)) {
            case null [];
            case (?bookingIds) {
                List.toArray(
                    List.mapFilter<BookingId, Booking>(
                        bookingIds,
                        func(id) {
                            Trie.get(bookings, key(id), Nat.equal);
                        },
                    )
                );
            };
        };
    };

    // System functions
    system func heartbeat() : async () {
        await checkBookingStatus();
    };

    private func checkBookingStatus() : async () {
        let currentTime = Time.now();

        for ((id, booking) in Trie.iter(bookings)) {
            if (booking.status == #confirmed and booking.checkOutDate < currentTime) {
                let updatedBooking = {
                    booking with
                    status = #completed;
                };

                bookings := Trie.put(
                    bookings,
                    key(id),
                    Nat.equal,
                    updatedBooking,
                ).0;

                switch (Trie.get(rooms, key(booking.roomId), Nat.equal)) {
                    case null {};
                    case (?room) {
                        let updatedRoom = {
                            room with
                            status = #available;
                        };

                        rooms := Trie.put(
                            rooms,
                            key(booking.roomId),
                            Nat.equal,
                            updatedRoom,
                        ).0;

                        availableRooms := Trie.put(
                            availableRooms,
                            key(booking.roomId),
                            Nat.equal,
                            (),
                        ).0;
                    };
                };
            };
        };
    };
};  