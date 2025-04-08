import Map "mo:base/OrderedMap";
import Time "mo:base/Time";
import List "mo:base/List";
import Nat "mo:base/Nat";
import Principal "mo:base/Principal";

persistent actor {

    public type JobListing = {
        id : Nat;
        title : Text;
        description : Text;
        budget : Float;
        client : Principal;
        status : Text; // "open", "in-progress", "completed"
    };

    public type Application = {
        jobId : Nat;
        freelancer : Principal;
        coverLetter : Text;
        bidAmount : Float;
    };

    public type Milestone = {
        id : Nat;
        description : Text;
        dueDate : Time.Time;
        status : Text; // "pending", "completed"
    };

    public type Review = {
        reviewer : Principal;
        reviewed : Principal;
        rating : Nat; // 1-5 scale
        comment : Text;
    };

    // Stable variables
    private transient let natMap : Map.Operations<Nat> = Map.Make<Nat>(Nat.compare);
    private var jobListings : Map.Map<Nat, JobListing> = natMap.empty<JobListing>();
    private var applications : Map.Map<Nat, List.List<Application>> = natMap.empty<List.List<Application>>();
    private var milestones : Map.Map<Nat, List.List<Milestone>> = natMap.empty<List.List<Milestone>>();
    private transient let principalMap : Map.Operations<Principal> = Map.Make<Principal>(Principal.compare);
    private var reviews : Map.Map<Principal, List.List<Review>> = principalMap.empty<List.List<Review>>();

    // create unique job IDs
    public func generateJobId() : async Nat {
        natMap.size<JobListing>(jobListings) + 1;
    };

    // Posting a job listing
    public shared ({ caller }) func postJobListing(title : Text, description : Text, budget : Float) : async JobListing {
        let jobId = await generateJobId();
        let job : JobListing = {
            id = jobId;
            title = title;
            description = description;
            budget = budget;
            client = caller;
            status = "open";
        };
        jobListings := natMap.put<JobListing>(jobListings, jobId, job);
        return job;
    };

    // Freelancer applies for a gig
    public shared ({ caller }) func applyForJob(jobId : Nat, coverLetter : Text, bidAmount : Float) : async Bool {
        switch (natMap.get<JobListing>(jobListings, jobId)) {
            case (?_job) {
                let application : Application = {
                    jobId = jobId;
                    freelancer = caller;
                    coverLetter = coverLetter;
                    bidAmount = bidAmount;
                };
                let existingApps : List.List<Application> = switch (natMap.get<List.List<Application>>(applications, jobId)) {
                    case (?apps) apps;
                    case (_) List.nil<Application>();
                };
                let updatedApps : List.List<Application> = List.push<Application>(application, existingApps);
                applications := natMap.put<List.List<Application>>(applications, jobId, updatedApps);
                return true;
            };
            case (_) return false;
        };
    };

    // Add a milestone to a job
    public func addMilestone(jobId : Nat, description : Text, dueDate : Time.Time) : async Milestone {
        let milestoneId = natMap.size<List.List<Milestone>>(milestones) + 1;
        let milestone : Milestone = {
            id = milestoneId;
            description = description;
            dueDate = dueDate;
            status = "pending";
        };
        let existingMilestones : List.List<Milestone> = switch (natMap.get<List.List<Milestone>>(milestones, jobId)) {
            case (?ms) ms;
            case (_) List.nil<Milestone>();
        };
        let updatedMilestones : List.List<Milestone> = List.push<Milestone>(milestone, existingMilestones);
        milestones := natMap.put<List.List<Milestone>>(milestones, jobId, updatedMilestones);
        return milestone;
    };

    // Update a milestone's status
    public func updateMilestoneStatus(jobId : Nat, milestoneId : Nat, status : Text) : async Bool {
        switch (natMap.get<List.List<Milestone>>(milestones, jobId)) {
            case (?milestoneList) {
                let updatedMilestoneList : List.List<Milestone> = List.map<Milestone, Milestone>(
                    milestoneList,
                    func(m) {
                        if (m.id == milestoneId) {
                            let milestone : Milestone = {
                                id = m.id;
                                description = m.description;
                                dueDate = m.dueDate;
                                status = status;
                            };
                            return milestone;
                        } else {
                            return m;
                        };
                    },
                );
                milestones := natMap.put<List.List<Milestone>>(milestones, jobId, updatedMilestoneList);
                return true;
            };
            case (_) return false;
        };
    };

    // Add a review
    public shared ({ caller }) func addReview(reviewed : Principal, rating : Nat, comment : Text) : async Bool {
        if (rating < 1 or rating > 5) {
            return false;
        };
        let review : Review = {
            reviewer = caller;
            reviewed = reviewed;
            rating = rating;
            comment = comment;
        };
        let existingReviews : List.List<Review> = switch (principalMap.get<List.List<Review>>(reviews, reviewed)) {
            case (?rvs) rvs;
            case (_) List.nil<Review>();
        };
        let updatedReviews : List.List<Review> = List.push<Review>(review, existingReviews);
        reviews := principalMap.put<List.List<Review>>(reviews, reviewed, updatedReviews);
        return true;
    };

    // Fetch all reviews for a user
    public func getReviewsForUser(user : Principal) : async List.List<Review> {
        switch (principalMap.get<List.List<Review>>(reviews, user)) {
            case (?userReviews) userReviews;
            case (_) List.nil<Review>();
        };
    };

    // Fetch applications for a job
    public func getApplicationsForJob(jobId : Nat) : async List.List<Application> {
        switch (natMap.get<List.List<Application>>(applications, jobId)) {
            case (?appList) appList;
            case (_) List.nil<Application>();
        };
    };

    // Fetch milestones for a job
    public func getMilestonesForJob(jobId : Nat) : async List.List<Milestone> {
        switch (natMap.get<List.List<Milestone>>(milestones, jobId)) {
            case (?milestoneList) milestoneList;
            case (_) List.nil<Milestone>();
        };
    };

};