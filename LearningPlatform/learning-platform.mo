import Array "mo:base/Array";
import Principal "mo:base/Principal";
import Result "mo:base/Result";
import Hash "mo:base/Hash";
import Text "mo:base/Text";
import Nat "mo:base/Nat";

persistent actor {

    // Course type to hold course details
    type Course = {
        id : Hash.Hash;
        name : Text;
        description : Text;
        totalLessons : Int;
    };

    // Progress type to hold user progress for a specific course
    type Progress = {
        userId : Principal;
        courseId : Hash.Hash;
        lessonsCompleted : Int;
    };

    // Stable variables to store courses and progress data
    private var courses : [Course] = [];
    private var progress : [Progress] = [];
    private var courseIdCounter : Nat = 0;

    func idHash(n : Nat) : Hash.Hash {
        Text.hash(Nat.toText(n));
    };

    // Add a new course
    public func addCourse(courseName : Text, courseDescription : Text, totalLessons : Int) : async Result.Result<Bool, Text> {
        courseIdCounter := courseIdCounter + 1;
        let courseId = idHash(courseIdCounter);
        let newCourse : Course = {
            id = courseId;
            name = courseName;
            description = courseDescription;
            totalLessons = totalLessons;
        };
        courses := Array.append(courses, [newCourse]);
        return #ok(true);
    };

    // Update progress for a user in a specific course
    public shared (msg) func updateProgress(courseId : Nat32, lessonsCompleted : Int) : async () {
        let existingProgress = Array.filter<Progress>(progress, func(p) { p.userId == msg.caller and p.courseId == courseId });

        if (Array.size(existingProgress) == 0) {
            let newProgress : Progress = {
                userId = msg.caller;
                courseId = courseId;
                lessonsCompleted = lessonsCompleted;
            };
            progress := Array.append(progress, [newProgress]);
        } else {
            let currentProgress = existingProgress[0];
            let updatedProgress = {
                currentProgress with lessonsCompleted = lessonsCompleted;
            };
            progress := Array.filter<Progress>(progress, func(p) { p.userId != msg.caller or p.courseId != courseId });
            progress := Array.append(progress, [updatedProgress]);
        };
    };

    // Get the progress of the current user for a specific course
    public shared (msg) func getProgress(courseId : Nat32) : async Result.Result<?Progress, Text> {
        let userProgress = Array.filter<Progress>(progress, func(p) { p.userId == msg.caller and p.courseId == courseId });
        switch (Array.size(userProgress) == 0) {
            case true return #err("No progress found for caller");
            case false return #ok(?userProgress[0]);
        };
    };

    // Check if the current user has completed a specific course
    public shared (msg) func checkCompletion(courseId : Nat32) : async Result.Result<Bool, Text> {
        let userProgress = Array.filter<Progress>(progress, func(p) { p.userId == msg.caller and p.courseId == courseId });
        switch (Array.size(userProgress) == 0) {
            case true return #err("No progress found for caller");
            case false {
                let course = Array.filter<Course>(courses, func(c) { c.id == courseId })[0];
                let progress = userProgress[0];
                return #ok(progress.lessonsCompleted == course.totalLessons);
            };
        };
    };

    // Get all courses
    public func getCourses() : async [Course] {
        return courses;
    };

    // Get a course by its ID
    public func getCourseById(courseId : Nat32) : async Result.Result<?Course, Text> {
        let course = Array.filter<Course>(courses, func(c) { c.id == courseId });
        switch (Array.size(course) == 0) {
            case true return #err("Course not found");
            case false return #ok(?course[0]);
        };
    };

    // Delete a course by its ID
    public func deleteCourse(courseId : Nat32) : async Result.Result<Bool, Text> {
        let courseExists = Array.filter<Course>(courses, func(c) { c.id == courseId });
        switch (Array.size(courseExists) == 0) {
            case true return #err("Course not found");
            case false {
                courses := Array.filter<Course>(courses, func(c) { c.id != courseId });
                progress := Array.filter<Progress>(progress, func(p) { p.courseId != courseId });
                return #ok(true);
            };
        };
    };
};