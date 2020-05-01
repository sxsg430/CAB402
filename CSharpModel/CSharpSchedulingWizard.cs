using Microsoft.FSharp.Core.CompilerServices;
using System.Collections.Generic;

namespace QUT
{
    using StudyPlan = IEnumerable<UnitInPlan>;

    public class CSharpSchedulingWizard
    {
        public static Semester currentSemester
        {
            // Do not change
            get { return new Semester(2020, Offering.Semester1); }
        }
        // Partially completed implementation of tryToCompleteBy, based on what I had written for the F# implementation. Had issues (errors trying to implement BoundPlan) and didn't have time to finish it.
        public StudyPlan tryToCompleteBy (Semester targetGraduation, StudyPlan plan)
        {
            if (targetGraduation.Equals(currentSemester))
            {
                yield break;
            } else
            {
                //boundPlan = BoundsOptimizer.boundUnitsInPlan(plan, currentSemester, targetGraduation);
                yield break;
            }
        }

        public static IEnumerable<StudyPlan> TryToImproveSchedule(StudyPlan plan)
        {
            Semester initialSemester = currentSemester;
            Semester lastSemester = FSharpSchedulingWizard.lastSemester(plan);

            return FSharpSchedulingWizard.TryToImproveSchedule(plan); // Didn't have time to complete, set to return the same as what the F# version does (so I can have some C# code written).
        }
    }
}