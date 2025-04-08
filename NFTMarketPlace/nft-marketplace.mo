import Cycles "mo:base/ExperimentalCycles";
import Principal "mo:base/Principal";
import Iter "mo:base/Iter";
import OrderedMap "mo:base/OrderedMap";
import Order "mo:base/Order";
import TrieSet "mo:base/TrieSet";

persistent actor OpenD {
    private type NFT = {
        itemName : Text;
        var nftOwner : Principal;
        imageBytes : [Nat8];
    };

    private func transferOwnership(nft : NFT, newOwner : Principal, caller : Principal) : Text {
        if (caller == nft.nftOwner) {
            nft.nftOwner := newOwner;
            return "Success";
        } else {
            return "Error: Not initiated by NFT Owner.";
        };
    };

    private type Listing = {
        itemOwner : Principal;
        itemPrice : Nat;
    };

    private func compare(p1 : Principal, p2 : Principal) : Order.Order {
        Principal.compare(p1, p2);
    };

    private transient let nftOps = OrderedMap.Make<Principal>(compare);
    var nfts = nftOps.empty<NFT>();
    private transient let ownerOps = OrderedMap.Make<Principal>(compare);
    var owners = ownerOps.empty<TrieSet.Set<Principal>>();
    private transient let listingOps = OrderedMap.Make<Principal>(compare);
    var listings = listingOps.empty<Listing>();

    public shared (msg) func mint(imgData : [Nat8], name : Text) : async Principal {
        let owner : Principal = msg.caller;
        Cycles.add<system>(100_500_000_000);
        let newNFT = {
            itemName = name;
            var nftOwner = owner;
            imageBytes = imgData;
        } : NFT;

        let newNFTPrincipal = msg.caller; // Generate a new Principal for NFT

        nfts := nftOps.put(nfts, newNFTPrincipal, newNFT);
        addToOwnershipMap(owner, newNFTPrincipal);

        return newNFTPrincipal;
    };

    private func addToOwnershipMap(owner : Principal, nftId : Principal) {
        var ownedNFTs : TrieSet.Set<Principal> = switch (ownerOps.get(owners, owner)) {
            case null TrieSet.empty<Principal>();
            case (?result) result;
        };

        ownedNFTs := TrieSet.put<Principal>(ownedNFTs, nftId, Principal.hash(nftId), Principal.equal);
        owners := ownerOps.put(owners, owner, ownedNFTs);
    };

    public query func getOwnedNFTs(user : Principal) : async [Principal] {
        var userNFTs : TrieSet.Set<Principal> = switch (ownerOps.get(owners, user)) {
            case null TrieSet.empty<Principal>();
            case (?result) result;
        };

        return TrieSet.toArray(userNFTs);
    };

    public query func getListedNFTs() : async [Principal] {
        let entries = listingOps.entries(listings);
        let ids = Iter.map(entries, func((k, _) : (Principal, Listing)) : Principal { k });
        return Iter.toArray(ids);
    };

    public shared (msg) func listItem(id : Principal, price : Nat) : async Text {
        var item : NFT = switch (nftOps.get(nfts, id)) {
            case null return "NFT does not exist.";
            case (?result) result;
        };

        let owner = item.nftOwner;
        if (Principal.equal(owner, msg.caller)) {
            let newListing : Listing = {
                itemOwner = owner;
                itemPrice = price;
            };
            listings := listingOps.put(listings, id, newListing);
            return "Success";
        } else {
            return "You don't own the NFT.";
        };
    };

    public query func getOpenDCanisterID() : async Principal {
        return Principal.fromActor(OpenD);
    };

    public query func isListed(id : Principal) : async Bool {
        listingOps.get(listings, id) != null;
    };

    public query func getOriginalOwner(id : Principal) : async Principal {
        switch (listingOps.get(listings, id)) {
            case null Principal.fromText("");
            case (?result) result.itemOwner;
        };
    };

    public query func getListedNFTPrice(id : Principal) : async Nat {
        switch (listingOps.get(listings, id)) {
            case null 0;
            case (?result) result.itemPrice;
        };
    };

    public shared (msg) func completePurchase(id : Principal, ownerId : Principal, newOwnerId : Principal) : async Text {
        var purchasedNFT : NFT = switch (nftOps.get(nfts, id)) {
            case null return "NFT does not exist";
            case (?result) result;
        };

        let transferResult = transferOwnership(purchasedNFT, newOwnerId, msg.caller);
        if (transferResult == "Success") {
            let (newListings, _) = listingOps.remove(listings, id);
            listings := newListings;
            var ownedNFTs : TrieSet.Set<Principal> = switch (ownerOps.get(owners, ownerId)) {
                case null TrieSet.empty<Principal>();
                case (?result) result;
            };
            ownedNFTs := TrieSet.delete<Principal>(ownedNFTs, id, Principal.hash(id), Principal.equal);
            owners := ownerOps.put(owners, ownerId, ownedNFTs);
            addToOwnershipMap(newOwnerId, id);
            return "Success";
        } else {
            return transferResult;
        };
    };
};
