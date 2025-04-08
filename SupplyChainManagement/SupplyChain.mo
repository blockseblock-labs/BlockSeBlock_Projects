import Time "mo:base/Time";
import List "mo:base/List";
import OrderedMap "mo:base/OrderedMap";
import Text "mo:base/Text";
import Nat "mo:base/Nat";
import Float "mo:base/Float";
import Principal "mo:base/Principal";
import Result "mo:base/Result";
import Array "mo:base/Array";
import Iter "mo:base/Iter";

persistent actor {
    type Item = {
        id : Nat;
        name : Text;
        sku : Text;
        quantity : Nat;
        location : Text;
        minStockLevel : Nat;
        price : Float;
    };

    type Order = {
        id : Nat;
        items : List.List<OrderItem>;
        status : OrderStatus;
        createdAt : Time.Time;
        updatedAt : Time.Time;
        expectedDelivery : Time.Time;
    };

    type OrderItem = {
        itemId : Nat;
        quantity : Nat;
        price : Float;
    };

    type OrderStatus = {
        #pending;
        #processing;
        #shipped;
        #delivered;
        #cancelled;
    };

    type Shipment = {
        id : Nat;
        orderId : Nat;
        origin : Text;
        destination : Text;
        status : ShipmentStatus;
        departureTime : ?Time.Time;
        arrivalTime : ?Time.Time;
    };

    type ShipmentStatus = {
        #preparing;
        #inTransit;
        #delivered;
        #delayed;
    };

    type Notification = {
        id : Nat;
        message : Text;
        ntype : NotificationType;
        createdAt : Time.Time;
        isRead : Bool;
    };

    type NotificationType = {
        #lowStock;
        #orderDelay;
        #shipmentDelay;
        #delivered;
    };

    private transient let itemMap = OrderedMap.Make<Nat>(Nat.compare);
    private transient let orderMap = OrderedMap.Make<Nat>(Nat.compare);
    private transient let shipmentMap = OrderedMap.Make<Nat>(Nat.compare);
    private transient let notificationMap = OrderedMap.Make<Principal>(Principal.compare);

    private var items : OrderedMap.Map<Nat, Item> = itemMap.empty<Item>();
    private var orders : OrderedMap.Map<Nat, Order> = orderMap.empty<Order>();
    private var shipments : OrderedMap.Map<Nat, Shipment> = shipmentMap.empty<Shipment>();
    private var notifications : OrderedMap.Map<Principal, List.List<Notification>> = notificationMap.empty<List.List<Notification>>();
    private var nextId : Nat = 0;

    // Inventory Management
    public shared (_msg) func addItem(name : Text, sku : Text, quantity : Nat, location : Text, minStockLevel : Nat, price : Float) : async Nat {
        let item : Item = {
            id = nextId;
            name = name;
            sku = sku;
            quantity = quantity;
            location = location;
            minStockLevel = minStockLevel;
            price = price;
        };
        items := itemMap.put(items, nextId, item);
        nextId += 1;
        item.id;
    };

    public shared (_msg) func updateStock(itemId : Nat, newQuantity : Nat) : async Result.Result<(), Text> {
        switch (itemMap.get(items, itemId)) {
            case null { #err("Item not found") };
            case (?item) {
                let updatedItem = {
                    id = item.id;
                    name = item.name;
                    sku = item.sku;
                    quantity = newQuantity;
                    location = item.location;
                    minStockLevel = item.minStockLevel;
                    price = item.price;
                };
                items := itemMap.put(items, itemId, updatedItem);
                #ok();
            };
        };
    };

    // Order Management
    public shared (_msg) func createOrder(orderItems : [(Nat, Nat)]) : async Result.Result<Nat, Text> {
        var itemsList = List.nil<OrderItem>();

        for ((itemId, quantity) in orderItems.vals()) {
            switch (itemMap.get(items, itemId)) {
                case null {
                    return #err("Item not found: " # Nat.toText(itemId));
                };
                case (?item) {
                    if (item.quantity < quantity) {
                        return #err("Insufficient stock for item: " # item.name);
                    };
                    itemsList := List.push(
                        {
                            itemId = itemId;
                            quantity = quantity;
                            price = item.price;
                        },
                        itemsList,
                    );
                };
            };
        };

        let order : Order = {
            id = nextId;
            items = itemsList;
            status = #pending;
            createdAt = Time.now();
            updatedAt = Time.now();
            expectedDelivery = Time.now() + (7 * 24 * 60 * 60 * 1_000_000_000);
        };

        orders := orderMap.put(orders, nextId, order);
        nextId += 1;
        #ok(order.id);
    };

    // Query functions for reports
    public query func getInventoryReport() : async [(Text, Nat)] {
        let itemEntries = Iter.toArray(itemMap.entries(items));
        Array.map<(Nat, Item), (Text, Nat)>(itemEntries, func(entry) { (entry.1.name, entry.1.quantity) });
    };

    // Shipment Management
    public shared (_msg) func createShipment(orderId : Nat, origin : Text, destination : Text) : async Result.Result<Nat, Text> {
        switch (orderMap.get(orders, orderId)) {
            case null { #err("Order not found") };
            case (?order) {
                if (order.status == #cancelled) {
                    return #err("Cannot create shipment for cancelled order");
                };

                let shipment : Shipment = {
                    id = nextId;
                    orderId = orderId;
                    origin = origin;
                    destination = destination;
                    status = #preparing;
                    departureTime = null;
                    arrivalTime = null;
                };

                shipments := shipmentMap.put(shipments, nextId, shipment);

                // Update order status
                let updatedOrder = {
                    id = order.id;
                    items = order.items;
                    status = #processing;
                    createdAt = order.createdAt;
                    updatedAt = Time.now();
                    expectedDelivery = order.expectedDelivery;
                };
                orders := orderMap.put(orders, orderId, updatedOrder);

                nextId += 1;
                #ok(shipment.id);
            };
        };
    };

    public shared (msg) func updateShipmentStatus(shipmentId : Nat, newStatus : ShipmentStatus) : async Result.Result<(), Text> {
        switch (shipmentMap.get(shipments, shipmentId)) {
            case null { #err("Shipment not found") };
            case (?shipment) {
                var departureTime = shipment.departureTime;
                var arrivalTime = shipment.arrivalTime;

                // Update timestamps based on status
                switch (newStatus) {
                    case (#inTransit) { departureTime := ?Time.now() };
                    case (#delivered) { arrivalTime := ?Time.now() };
                    case (_) {};
                };

                let updatedShipment : Shipment = {
                    id = shipment.id;
                    orderId = shipment.orderId;
                    origin = shipment.origin;
                    destination = shipment.destination;
                    status = newStatus;
                    departureTime = departureTime;
                    arrivalTime = arrivalTime;
                };

                shipments := shipmentMap.put(shipments, shipmentId, updatedShipment);

                // Update order status if shipment is delivered
                if (newStatus == #delivered) {
                    switch (orderMap.get(orders, shipment.orderId)) {
                        case null {};
                        case (?order) {
                            let updatedOrder = {
                                id = order.id;
                                items = order.items;
                                status = #delivered;
                                createdAt = order.createdAt;
                                updatedAt = Time.now();
                                expectedDelivery = order.expectedDelivery;
                            };
                            orders := orderMap.put(orders, order.id, updatedOrder);
                            await createNotification(msg.caller, "Order #" # Nat.toText(order.id) # " has been delivered", #delivered);
                        };
                    };
                };

                // Create notification for delay
                if (newStatus == #delayed) {
                    await createNotification(msg.caller, "Shipment #" # Nat.toText(shipmentId) # " has been delayed", #shipmentDelay);
                };

                #ok();
            };
        };
    };

    // Notification Management
    public shared (_msg) func createNotification(recipient : Principal, message : Text, nType : NotificationType) : async () {
        let notification : Notification = {
            id = nextId;
            message = message;
            ntype = nType;
            createdAt = Time.now();
            isRead = false;
        };

        let existingNotifications = switch (notificationMap.get(notifications, recipient)) {
            case null { List.nil<Notification>() };
            case (?notifs) { notifs };
        };

        let updatedNotifications = List.push(notification, existingNotifications);
        notifications := notificationMap.put(notifications, recipient, updatedNotifications);
        nextId += 1;
    };

    public shared query (msg) func getNotifications() : async [Notification] {
        switch (notificationMap.get(notifications, msg.caller)) {
            case null { [] };
            case (?notifs) { List.toArray(notifs) };
        };
    };

    public shared (msg) func markNotificationAsRead(notificationId : Nat) : async Result.Result<(), Text> {
        switch (notificationMap.get(notifications, msg.caller)) {
            case null { #err("No notifications found") };
            case (?notifs) {
                let updatedNotifs = List.map<Notification, Notification>(
                    notifs,
                    func(n : Notification) : Notification {
                        if (n.id == notificationId) {
                            {
                                id = n.id;
                                message = n.message;
                                ntype = n.ntype;
                                createdAt = n.createdAt;
                                isRead = true;
                            };
                        } else { n };
                    },
                );
                notifications := notificationMap.put(notifications, msg.caller, updatedNotifs);
                #ok();
            };
        };
    };

    public query func getShipment(shipmentId : Nat) : async ?Shipment {
        shipmentMap.get(shipments, shipmentId);
    };

    public query func getShipmentsByOrder(orderId : Nat) : async [Shipment] {
        let allShipments = Iter.toArray(shipmentMap.vals(shipments));
        Array.filter(allShipments, func(s : Shipment) : Bool { s.orderId == orderId });
    };

    public query func getDelayedShipments() : async [Shipment] {
        let allShipments = Iter.toArray(shipmentMap.vals(shipments));
        Array.filter(allShipments, func(s : Shipment) : Bool { s.status == #delayed });
    };

    public query func getShipmentReport() : async [(Nat, ShipmentStatus)] {
        let shipmentEntries = Iter.toArray(shipmentMap.entries(shipments));
        Array.map<(Nat, Shipment), (Nat, ShipmentStatus)>(
            shipmentEntries,
            func((id, shipment)) { (id, shipment.status) },
        );
    };

    public query func getOrderStatusReport() : async [(Nat, OrderStatus)] {
        let orderEntries = Iter.toArray(orderMap.entries(orders));
        Array.map<(Nat, Order), (Nat, OrderStatus)>(
            orderEntries,
            func((id, order)) { (id, order.status) },
        );
    };
};