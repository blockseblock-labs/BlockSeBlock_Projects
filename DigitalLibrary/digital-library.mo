import Time "mo:base/Time";
import List "mo:base/List";
import Trie "mo:base/Trie";
import Text "mo:base/Text";
import Principal "mo:base/Principal";
import Iter "mo:base/Iter";
import Nat "mo:base/Nat";
import Nat32 "mo:base/Nat32";

persistent actor DigitalLibrary {

    // Book structure definition
    type Book = {
        id : Nat;
        isbn : Text;
        title : Text;
        author : Text;
        genre : Text;
        available : Bool;
        reservedBy : ?Principal;
        dueDate : ?Int;
    };

    // User structure definition
    type User = {
        borrowedBooks : List.List<Nat>;
        lateFees : Nat;
    };

    // Stable variables
    private var books = Trie.empty<Nat, Book>();
    private var users = Trie.empty<Principal, User>();
    private var nextBookId : Nat = 0;

    // Helper function to hash natural numbers
    private func natHash(n : Nat) : Nat32 {
        Text.hash(Nat.toText(n));
    };

    /// Adds a new book to the library with an ISBN
    public func addBook({
        bookISBN : Text;
        bookTitle : Text;
        bookAuthor : Text;
        bookGenre : Text;
    }) : async Text {
        let newBook : Book = {
            id = nextBookId;
            isbn = bookISBN;
            title = bookTitle;
            author = bookAuthor;
            genre = bookGenre;
            available = true;
            reservedBy = null;
            dueDate = null;
        };
        nextBookId += 1;

        books := Trie.put(books, { key = newBook.id; hash = natHash(newBook.id) }, Nat.equal, newBook).0;

        return Nat.toText(newBook.id);
    };

    public query func searchBooks({ searchQuery : Text }) : async [Book] {
        let booksIter = Iter.map(
            Trie.iter(books),
            func((_id : Nat, book : Book)) : Book {
                book;
            },
        );

        let filteredBooksIter = Iter.filter(
            booksIter,
            func(book : Book) : Bool {
                // Case-insensitive search
                Text.contains(Text.toLowercase(book.title), #text(Text.toLowercase(searchQuery))) or Text.contains(Text.toLowercase(book.author), #text(Text.toLowercase(searchQuery))) or Text.contains(Text.toLowercase(book.genre), #text(Text.toLowercase(searchQuery)));
            },
        );

        return Iter.toArray(filteredBooksIter);
    };

    /// Allows a user to borrow a book
    public shared ({ caller }) func checkoutBook({ id : Nat }) : async Text {
        let currentUser = switch (Trie.get(users, { key = caller; hash = Principal.hash(caller) }, Principal.equal)) {
            case null { { borrowedBooks = List.nil(); lateFees = 0 } };
            case (?existingUser) existingUser;
        };

        switch (Trie.get(books, { key = id; hash = natHash(id) }, Nat.equal)) {
            case (?book) {
                if (book.available and book.reservedBy == null) {
                    let updatedBook = {
                        book with available = false;
                        dueDate = ?(Time.now() + 604800_000_000_000_000); // 7 days
                    };

                    let updatedUser = {
                        currentUser with borrowedBooks = List.push(id, currentUser.borrowedBooks)
                    };

                    books := Trie.put(books, { key = id; hash = natHash(id) }, Nat.equal, updatedBook).0;
                    users := Trie.put(users, { key = caller; hash = Principal.hash(caller) }, Principal.equal, updatedUser).0;

                    return "Book checked out successfully!";
                } else {
                    return "Book is not available for checkout.";
                };
            };
            case _ { return "Book not found." };
        };
    };

    /// Allows a user to return a borrowed book
    public shared ({ caller }) func returnBook({ id : Nat }) : async Text {
        switch (Trie.get(users, { key = caller; hash = Principal.hash(caller) }, Principal.equal)) {
            case (?user) {
                if (List.some(user.borrowedBooks, func(borrowedID : Nat) : Bool { borrowedID == id })) {
                    switch (Trie.get(books, { key = id; hash = natHash(id) }, Nat.equal)) {
                        case (?book) {
                            let isLate = switch (book.dueDate) {
                                case (?dueDate) Time.now() > dueDate;
                                case _ false;
                            };

                            let updatedUser = {
                                user with borrowedBooks = List.filter(user.borrowedBooks, func(borrowedID : Nat) : Bool { borrowedID != id });
                                lateFees = user.lateFees + (if (isLate) { 10 } else { 0 });
                            };

                            let updatedBook = {
                                book with available = true;
                                dueDate = null;
                                reservedBy = null;
                            };

                            users := Trie.put(users, { key = caller; hash = Principal.hash(caller) }, Principal.equal, updatedUser).0;
                            books := Trie.put(books, { key = id; hash = natHash(id) }, Nat.equal, updatedBook).0;

                            return "Book returned successfully.";
                        };
                        case _ { return "Book not found in system." };
                    };
                } else {
                    return "You haven't borrowed this book.";
                };
            };
            case null { return "User not found." };
        };
    };

    /// Retrieves the late fees for the current user
    public query ({ caller }) func getLateFees() : async Nat {
        switch (Trie.get(users, { key = caller; hash = Principal.hash(caller) }, Principal.equal)) {
            case (?user) user.lateFees;
            case null 0;
        };
    };
};