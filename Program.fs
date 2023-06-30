open System.Collections.Generic
open System.IO
open System.Text
open FunWithGit
open LibGit2Sharp
open System





// let buildGraph () =
//     use repository = new Repository("../electron-api-demos")

//     //
//     // let rec prong (commit : Commit) count =
//     //     // Get first parent under commit
//     //     // Is parent next commit in line ->
//     //     let parents = commit.Parents |> Array.ofSeq
//     //     printfn "%s %i" commit.MessageShort parents.Length
//     //     if count > 20 then
//     //         ()
//     //     else
//     //         prong parents[0] (count + 1)
//     //         // printfn "commit %s"

//     // prong repository.Head.Tip 0

//     repository.Commits |> Seq.take 100 |> Seq.iter (printfn "%A")

//     let isSameBranch (current: Commit) (next: Commit) =
//         let parent = current.Parents |> Seq.head
//         parent = next

//     let poop (repository: Repository) =

//         let enumerator = repository.Commits.GetEnumerator()



//             // If next is parent then we are on the same branch
//             // If next is not parent then we need to start a new branch


//         let rec createRow iterationNumber  previousLaneIndex (enumerator : Commit IEnumerator) =

//             let current = enumerator.Current


//             let laneIndex = if isSameBranch current next then previousLaneIndex else previousLaneIndex + 1
//             let row = Array.create (laneIndex + 1) false
//             row[laneIndex] <- true
//             if not (enumerator.MoveNext()) || iterationNumber = 100  then
//                 [row]
//             else
//                 let next = enumerator.Current
//                 row :: createRow (iterationNumber + 1) laneIndex enumerator



//         ()




//     for branch in repository.Branches do
//         printfn "B %s" branch.FriendlyName

//     // repository.Commits.QueryBy(CommitFilter())
//     //     |> Seq.take 100
//     //     |> Seq.map (fun commit -> commit.MessageShort, commit.Parents |> Seq.length)
//     //     |> Seq.iter (printfn "%A")

//     printfn "Complete"
//     ()


let paths =
    [| Path.Combine(__SOURCE_DIRECTORY__, "../auster")
       Path.Combine(__SOURCE_DIRECTORY__, "../electron-api-demos") |]
// buildGraph()
use repository = new Repository(paths[1])
//
// let limit = 100
//
//
// let commits =
//     // repository.Commits.QueryBy(CommitFilter()) |> Seq.take limit |> Array.ofSeq
//     repository.Commits.QueryBy(CommitFilter()) |> Seq.take limit |> Array.ofSeq
// // Can not get a list of all commits in chronological order
// let orderedBranches = repository.Branches  |> Array.ofSeq |> Array.filter (fun branch -> not branch.IsRemote) |> Array.sortByDescending (fun branch -> branch.Tip.Committer.When)
// orderedBranches |> Array.iter (fun branch -> printfn $"%A{branch.FriendlyName} {branch.CanonicalName}")
//
//
// let mutable lanes = [ (0, 0) ]
// let mutable activeLane = 0
//
// for index = 0 to orderedBranches.Length - 1 do
//     let branch = orderedBranches[index]
//     ()
//
// let isSameBranch firstParent (next: Commit) = firstParent = next
//
// let builder = StringBuilder()
//
// // Branch counts represent the current amount of branches from top to bottom. So merge commits create new branches and splits close them.
// let mutable branchCount = 0
//
//
// for index = 0 to limit - 1 do
//     let commit = commits[index]
//     let parents = commit.Parents |> Array.ofSeq
//
//     let isMergeCommit = parents |> Array.length |> (<) 1
//
//     if isMergeCommit then
//         branchCount <- branchCount + 1
//
//     if index + 1 = limit || isSameBranch parents[0] commits[index + 1] then
//         let row = String.replicate branchCount " "
//         builder.Append(row).Append("|").AppendLine() |> ignore
//     else
//         branchCount <- branchCount + 1
//         let row = String.replicate branchCount " "
//         builder.Append(row).Append("|").AppendLine() |> ignore
//
//
// Console.WriteLine(builder.ToString())
//
//
//
//
//
let commits = CommitGraph.listCommits repository |> Seq.take 10 |> Seq.toArray


let sorted = CommitGraph.sortTemporalTopological commits
CommitGraph.curvedBranches sorted
