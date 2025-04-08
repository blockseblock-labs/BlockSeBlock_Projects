import Debug "mo:base/Debug";
import Error "mo:base/Error";
import Iter "mo:base/Iter";
import Map "mo:base/OrderedMap";
import Nat "mo:base/Nat";
import Principal "mo:base/Principal";
import Set "mo:base/OrderedSet";

persistent actor {
    type Task = TaskModule.Task;
    type User = UserModule.User;

    private transient let natMap = Map.Make<Nat>(Nat.compare);
    private transient let natSet = Set.Make<Nat>(Nat.compare);
    private transient let principalMap = Map.Make<Principal>(Principal.compare);

    private var nextTaskID : Nat = 0;
    private var tasks : Map.Map<Nat, Task> = natMap.empty<Task>();
    private var users : Map.Map<Principal, User> = principalMap.empty<User>();

    module TaskModule {
        public type Task = {
            id : Nat;
            title : Text;
            description : Text;
            completed : Bool;
        };

        public func initializeTask(title : Text, description : Text) : Task {
            let task = {
                id = nextTaskID;
                title = title;
                description = description;
                completed = false;
            };
            nextTaskID += 1;
            task;
        };
    }; // End TaskModule.

    module UserModule {
        public type User = {
            principal : Principal;
            var taskList : Set.Set<Nat>;
        };

        public func getUserTasks(user : User) : [Task] {
            let taskIds = natSet.vals(user.taskList);
            let tempTasks = Iter.map<Nat, Task>(
                taskIds,
                func taskId {
                    switch (natMap.get(tasks, taskId)) {
                        case (?task) task;
                        case null Debug.trap("Task with id " # Nat.toText(taskId) # " does not exist. This shouldn't happen.");
                    };
                },
            );
            Iter.toArray(tempTasks);
        };

        public func getOrInitializeUser(principal : Principal) : User {
            switch (principalMap.get(users, principal)) {
                case (?user) user;
                case null {
                    {
                        principal = principal;
                        var taskList = natSet.empty();
                    };
                };
            };
        };
    }; // End UserModule.

    public shared ({ caller }) func createTask(title : Text, description : Text) : async Nat {
        if (Principal.isAnonymous(caller)) {
            throw Error.reject("An anonymous user is not allowed to add task.");
        };

        let newTask = TaskModule.initializeTask(title, description);
        tasks := natMap.put(tasks, newTask.id, newTask);

        let user = UserModule.getOrInitializeUser(caller);
        user.taskList := natSet.put(user.taskList, newTask.id);
        users := principalMap.put(users, caller, user);

        newTask.id;
    };

    public shared ({ caller }) func markTaskAsCompleted(id : Nat) : async () {
        if (Principal.isAnonymous(caller)) {
            throw Error.reject("An anonymous user is not allowed to remove task.");
        };

        let ?user = principalMap.get(users, caller) else throw Error.reject("User with principal " # Principal.toText(caller) # " does not exist.");

        if (not natSet.contains(user.taskList, id)) {
            throw Error.reject("Task " # Nat.toText(id) # " is not in the tasklist of user " # Principal.toText(caller) # ".");
        };

        let ?task = natMap.get(tasks, id) else throw Error.reject("Task with id " # Nat.toText(id) # " does not exist.");
        let completedTask = { task with completed = true };
        tasks := natMap.put(tasks, id, completedTask);
    };

    public shared ({ caller }) func removeTaskById(id : Nat) : async () {
        if (Principal.isAnonymous(caller)) {
            throw Error.reject("An anonymous user is not allowed to remove task.");
        };

        if (not natMap.contains(tasks, id)) {
            throw Error.reject("Task with id " # Nat.toText(id) # " does not exist.");
        };

        let ?user = principalMap.get(users, caller) else throw Error.reject("User with principal " # Principal.toText(caller) # " does not exist.");

        if (not natSet.contains(user.taskList, id)) {
            throw Error.reject("Task " # Nat.toText(id) # " is not in the tasklist of user " # Principal.toText(caller) # ".");
        };

        tasks := natMap.delete(tasks, id);
        user.taskList := natSet.delete(user.taskList, id);
        users := principalMap.put(users, caller, user);
    };

    public shared query ({ caller }) func getUserTasks(isCompletedMask : ?Bool) : async [Task] {
        let ?user = principalMap.get(users, caller) else throw Error.reject("User with principal " # Principal.toText(caller) # " does not exist.");
        let tasks = UserModule.getUserTasks(user);
        let tasksIter = Iter.fromArray(tasks);

        let filteredTasks = Iter.filter<Task>(
            tasksIter,
            func task {
                switch (isCompletedMask) {
                    case (?completed) task.completed == completed;
                    case null true;
                };
            },
        );
        Iter.toArray(filteredTasks);
    };

};