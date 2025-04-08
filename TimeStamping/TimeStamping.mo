import Time "mo:base/Time";
import Map "mo:base/OrderedMap";
import Blob "mo:base/Blob";
import Nat64 "mo:base/Nat64";

persistent actor TimeStamping {
    type Timestamp = Nat64;

    type TimestampedRecord = {
        description : Text;
        timestamp_seconds : Timestamp;
    };

    type Record = {
        description : Text;
        identifier : Blob;
    };

    private transient let blobMap = Map.Make<Blob>(Blob.compare);
    private var records = blobMap.empty<TimestampedRecord>();

    public func record(record : Record) : async ?Timestamp {
        if (record.identifier.size() > 64) {
            return null;
        };

        switch (blobMap.get(records, record.identifier)) {
            case (?_) null;
            case null {
                let currentTime = Nat64.fromIntWrap(Time.now() / 1_000_000_000);
                let timestampedRecord : TimestampedRecord = {
                    record with timestamp_seconds = currentTime
                };
                records := blobMap.put(records, record.identifier, timestampedRecord);
                ?currentTime;
            };
        };
    };

    public query func retrieve(identifier : Blob) : async ?TimestampedRecord {
        return blobMap.get(records, identifier);
    };
};