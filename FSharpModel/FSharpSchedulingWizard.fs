module FSharpSchedulingWizard

open QUT
open StudyPlannerModel

// Functions used for optimizing study plan ...

// The semester that we are currently in
let currentSemester : Semester = 
    // Do not change
    { year = 2020; offering = Semester1 }

    
// Support Function: Internal function for checking if a given semester has 3 or less entries in the plan and returns the Semester if true. False values are ignored (doesn't cause any issues).
let matchSemesterAmount semesterList = // semList should be a list with the elements in the form of [({year = 2020; offering = Semester1}, 3)], indicating that the first semester of 2020 occurs 3 times in the list.
    match semesterList with
    | (var1, var2) when var2 <=3 -> var1 // Return just the Semester portion (var1) if the number of appearances (var2) is less than 4.

// Given a partial plan, try to schedule the remaining units such that all remaining units are legally scheduled 
// (with no more than 4 units per semester) .
// Should return None if it is not possible to schedule the remaining units
// We start by selecting one of the remaining units that can be scheduled in at least one of its possible semesters.
// If none of the remaining units can be scheduled then we fail.
// Otherwise we try scheduling that unit in each of the possible semesters in which it can be legally scheduled. 
// If any of those schedules can be extended into a complete plan then we succeed, otherwise we fail.
let rec private scheduleRemaining (remainingUnits:BoundPlan) (plannedUnits:StudyPlan): StudyPlan option =
    if List.length remainingUnits = 0 then // If the length of the provided Bound Plan is 0, assume it is empty and return the current Study Plan.
        Option.Some plannedUnits
    else
        
        let boundPlanHeadElement = remainingUnits.Head // Obtain the first unit in the BoundPlan by taking its head value.
        let semesterCount =  boundPlanHeadElement.possibleSemesters |> Seq.countBy id // Construct a new sequence of the type Semester * Int. and pass it the list of Possible Semesters from the saved head element. Results are in the form (Semester, Occurances) for later use when parsing.
        let sortedSemesters = Seq.map (fun x -> matchSemesterAmount x) semesterCount // New sequence containing the values of the Sequence above run through the match support function which returns the Semester elements that occur in the head unit. Didn't quite do what I initially thought it would do when I wrote it but never rewrote any code that depended on it.


        let plannedUnitSemesters = Seq.filter (fun x -> isEnrollableIn boundPlanHeadElement.code x plannedUnits) sortedSemesters // Filter the list of sorted semesters by which semester the selected head unit can be enrolled in.

        if Seq.length plannedUnitSemesters > 0 then // If the length of the sequence of semesters containing the enrollable semesters has any contents, continue.
            let newUnit : seq<UnitInPlan> = Seq.singleton { code = boundPlanHeadElement.code; studyArea = boundPlanHeadElement.studyArea; semester = Seq.max plannedUnitSemesters} // Construct new 'UnitInPlan' for appending to the Study Plan.

            let newBoundPlan : BoundPlan = remainingUnits |> List.rev |> List.take (List.length remainingUnits - 1) |> List.rev // An attempt at removing the head element from the Bound Plan. Reverses the plan, takes all elements but the last one and reverses the list again to obtain (hopefully) the list without its first value.

            let newStudyPlan : StudyPlan = plannedUnits |> Seq.map (fun x -> x) |> Seq.append newUnit // Create a new study plan by appending the new UnitInPlan to the existing Plan.
            scheduleRemaining newBoundPlan newStudyPlan
        else
            Option.None // Return Option.None as required if no units are available for scheduling.

// Assuming that study commences in the given first semester and that units are only studied 
// in semester 1 or semester 2, returns the earliest possible semester by which all units in
// the study plan could be completed, assuming at most 4 units per semester.
let private bestAchievable (firstSemester:Semester) (plan:StudyPlan) : Semester =
     let unitsInPlan = float(Seq.length plan) // Obtain the float value of the length of the provided Study Plan.
     let semesterCount = int(ceil(unitsInPlan / 4.0)) //Divide the length of the Study Plan by 4 (max number of units per semester), round it up to the nearest whole number and convert it back into an integer for later use.
     let finalSequence = SemesterSequence firstSemester {year=2026; offering=Semester2} |> Seq.filter (fun x -> x.offering.Equals Semester1 || x.offering.Equals Semester2) |> Seq.take semesterCount |> Seq.max
     // Generate a sequence of Semesters using the provided first Semester and one far beyond what any of the plans require. Filter the sequence so only entries for Semester 1 & 2 are present (no plans have units occuring in Summer). Only keep the entries in the list up to the number of units and take the maximum value.
     // Allows us to identify the best achievable semester based on the amonut required (excluding Summer).
     finalSequence

// Returns the last semester in which units will be studied in the study plan
let lastSemester (plan: StudyPlan): Semester =
     Seq.map (fun x -> x.semester) plan |> Seq.max // Construct a map of the semester values contained within the StudyPlan and obtain the max value. Used to obtain the last semester contained within a plan where units are being studied.



// Returns true if and only if every unit in the plan has at least one possible semester for it to be scheduled
let allBoundsFeasible (bounds:BoundPlan) =
    // do not change  (difficulty: 3/10)
    bounds |> Seq.forall (fun unit -> not (Seq.isEmpty unit.possibleSemesters)) 

// Returns a sequence of progressively better study plans.
// Each successive plan returned finishes in an earlier semester than the previous plan.
// Should return the empty sequence if the plan cannot be improved.
// The earliest semester that we can schedule units in is the current semester.
// Successively better plans are created by specifying progressively tighter target graduation semesters.
// We determine the final semester of the current plan and set our target semester as the semester before that.
// If we succeed in finding a plan that completes by that target semester then we try to improve that plan further, 
// semester by semester until it becomes impossible to improve further.
let TryToImproveSchedule (plan:StudyPlan) : seq<StudyPlan> =
    let first = currentSemester
    let last = lastSemester plan
    let bestPossible = bestAchievable first plan
    let rec TryToCompleteBy (targetGraduation:Semester) =
        if targetGraduation = currentSemester then // If the provided target graduation is equal to the defined currentSemester, return an empty Sequence.
            Seq.empty
        else
            let initialBoundPlan = BoundsOptimizer.boundUnitsInPlan plan currentSemester targetGraduation // Obtain a BoundPlan by passing the current StudyPlan, first and last semesters to the Bound Plan generator in Bounds Optimiser (still using the naive implementation).
            let initialElement = Seq.head plan
            let cleanStudyPlan : StudyPlan = Seq.singleton initialElement // Create a "clean" Study Plan containing just the first element of the original plan.
            let remainingUnit = scheduleRemaining initialBoundPlan plan // Invoke Schedule Remaining with the bound plan and "clean" study plan and store the result.
            if remainingUnit.IsNone then // If Schedule Remaining returns None or if the target graduation is equal to the best achievable semester based on the current provided semester and plan, return an empty sequence.
                Seq.empty
            elif targetGraduation.Equals bestPossible then
                Seq.empty
            else
                TryToCompleteBy (previousSemester targetGraduation) // Otherwise, recursively call TryToCompleteBy again for the next set of results.
    TryToCompleteBy (previousSemester last)
