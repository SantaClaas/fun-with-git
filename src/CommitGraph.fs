/// <summary>
/// Attempt to implement https://pvigier.github.io/2019/05/06/commit-graph-drawing-algorithms.html
/// </summary>
module FunWithGit.CommitGraph

open System
open System.Collections.Generic
open System.Diagnostics
open LibGit2Sharp

[<CustomEquality; NoComparison; DebuggerDisplay "{messageShort}">]
type Commit =
    {
        mutable parents: Commit list
        mutable children: Commit list
        id: string
        messageShort: string
        /// <summary>
        /// Date committed, not authored
        /// </summary>
        date: DateTimeOffset
    }

    interface IEquatable<Commit> with
        member this.Equals other = other.id.Equals this.id

    override this.Equals other =
        match other with
        | :? Commit as commit -> (this :> IEquatable<_>).Equals commit
        | _ -> false


    override this.GetHashCode() = this.id.GetHashCode()
// Another attempt
let anotherIdeaDump (repository: IRepository) =
    // When all the parents of a child are in children by parent, delete the child parents
    // When the parent retrieves their children, delete the entry for the parent

    // Get the first commit by date
    // If the current commit is in positions, stop following the branch, because this is a branching we encountered
    // which gets already followed by another branch and commits would otherwise appear duplicate

    ()

// [<CustomEquality; NoComparison; DebuggerDisplay "{messageShort}">]
// type Crommit =
//     { id: string
//       i: int
//       mutable j: int option
//       branchChildren: Crommit list
//       mergeChildren: Crommit list
//       messageShort: string
//       parents: Crommit list }
//
//     interface IEquatable<Crommit> with
//         member this.Equals other = other.id.Equals this.id
//
//     override this.Equals other =
//         match other with
//         | :? Commit as commit -> (this :> IEquatable<_>).Equals commit
//         | _ -> false
//
//
//     override this.GetHashCode() = this.id.GetHashCode()


let sortTemporalTopological (commits: Commit array) =
    let mutable ordered = List.empty
    let exploredCommits = HashSet()

    let rec depthFirstSearch (commit: Commit) =
        if commit |> exploredCommits.Contains |> not then
            exploredCommits.Add commit |> ignore

            for descendant in commit.children do
                depthFirstSearch descendant

            ordered <- ordered @ [ commit ]


    for commit in commits |> Seq.sortByDescending (fun commit -> commit.date) do
        depthFirstSearch commit

    ordered

/// <summary>
/// Is <paramref name="b"/> a branch child of <paramref name="a"/>
/// </summary>
let isBranchChildOf (a: Commit) (b: Commit) =
    b.parents |> List.length > 0 && b.parents[0].id = a.id

/// <summary>
/// Is <paramref name="b"/> a merge child of <paramref name="a"/>
/// </summary>
let isMergeChildOf (a: Commit) (b: Commit) =
    b.parents |> List.length > 0 && b.parents[0].id <> a.id

let branchChildrenOf (commit: Commit) =
    commit.children |> Seq.filter (isBranchChildOf commit) |> Seq.toArray

let mergeChildrenOf (commit: Commit) =
    commit.children |> Seq.filter (isMergeChildOf commit) |> Seq.toArray

let curvedBranches (orderedCommits: Commit list) =

    seq {
        let branches = List()

        for i = 0 to (orderedCommits |> List.length) - 1 do
            let commit = orderedCommits[i]
            let branchChildren = branchChildrenOf commit

            let j =
                if branchChildren.Length <> 0 then

                    // We follow the first (or latest as in time/with the biggest date) branch that has our current branch as as parent
                    // This should increase straightness of branches because of smaller gaps
                    let bran =
                        branches
                        |> Seq.filter (fun branch -> branchChildren |> Array.contains branch)
                        |> Seq.maxBy (fun commit -> commit.date)

                    let index = branches.LastIndexOf bran

                    // "select d in c.branchChildren"
                    // "replace d by c in B"
                    for index = 0 to branches.Count - 1 do
                        for descendant in branchChildren do
                            let branchCommit = branches[index]

                            if branchCommit = descendant then
                                branches[index] <- commit

                    index

                else
                    branches.Add commit
                    branches.Count - 1

            for descendant in branchChildren |> Seq.except branchChildren do
                branches.Remove descendant |> ignore

            yield i, j, commit
    }

/// <summary>
/// Computes forbidden j-coordinates for commit c
/// </summary>
// let computeForbiddenIndices c (activeNodes: Map<string, Set<int>>) =
//     // Find forbidden indices for highest child
//     match c.mergeChildren with
//     | [] -> None
//     | [ mergeChild ] ->
//         // Highest child is the only child so i min is of that child
//         activeNodes |> Map.tryFind mergeChild.id
//     | mergeChildren ->
//         let highestChild = mergeChildren |> List.minBy (fun d -> d.i)
//         activeNodes |> Map.tryFind highestChild.id
//     |> Option.defaultValue Set.empty

// List helper to "replace at". Very inefficient
let replaceAt index item list =
    list |> List.mapi (fun index' item' -> if index' = index then item else item')
//
// let straightBranches (C: Crommit list) =
//
//     let activeNodes = Map.empty
//     // Initialize an empty list of active branches B
//     let mutable B: Crommit option list = []
//
//     // for c in C from lowest i-coordinate to largest
//     for c in C |> List.sortByDescending (fun c -> c.i) do
//         // compute forbidden j-coordinates J(c)
//         let Jc = computeForbiddenIndices c activeNodes
//         // if {d in c.branchChildren s.t. d.j is not it J(c)}
//         let allowedDescendants =
//             c.branchChildren
//             |> List.filter (fun d -> d.j |> Option.exists (fun dj -> Jc |> Set.contains dj |> not))
//
//         if
//             allowedDescendants
//             // is not empty
//             |> List.isEmpty
//             |> not
//         then
//             // select d in {d in c.branchChildren s.t. d.j is not in J(c)}
//             for d in allowedDescendants do
//                 // replace d by c in B
//                 // match B |> List.tryFindIndex (fun b -> b |> Option.exists (fun b -> b = d)) with
//                 // | None ->
//                 //     printfn "Shouldn't happen"
//                 //     ()
//                 // | Some index ->
//                 //     // Replace at
//                 //     B <- B |> replaceAt index (Some c)
//                 let index' = B |> List.findIndex (Option.exists (fun b -> b = d))
//                 B <- B |> replaceAt index' (Some c)
//         else
//             // insert c in B
//             B <- B @ [ Some c ]
//
//         for d' in
//             c.branchChildren
//             |> List.filter (fun d' -> allowedDescendants |> List.contains d' |> not) do
//
//             d'.j |> Option.iter (fun d'j -> B <- B |> replaceAt d'j None)
//
//
//         c.j <- B |> List.findIndex (Option.exists (fun b -> b = c)) |> Some


let listCommits (repository: IRepository) =
    let branches =
        repository.Branches
        // We use the commits as branches
        |> Seq.map (fun branch -> branch.Commits.GetEnumerator())
        // Filter out dead branches
        |> Seq.filter (fun branch -> branch.MoveNext())
        // Remove duplicates for this traversal they are duplicates if they have the same start commit
        |> Seq.distinctBy (fun branch -> branch.Current.Sha)
        // The time can be incorrect and the topological order doesn't follow the time stamp since time can be set to anything
        // But for the initial set of branches that might still be open and not merged we can order them by time
        |> Seq.sortByDescending (fun branch -> branch.Current.Committer.When)
        |> List

    // Keep track of commits we have yielded to not repeat.
    // And stop following branches that branch out from other branches.
    let yieldedById: Dictionary<string, Commit> = Dictionary()

    let childrenByParent: Dictionary<LibGit2Sharp.Commit, Commit list> = Dictionary()
    // Find latest commit
    let getLatestCommit () =
        let mutable latestBranch =
            branches |> Seq.maxBy (fun branch -> branch.Current.Committer.When)

        // If it was already yielded, then we encountered the branching of a branch and remove the branch
        while yieldedById.ContainsKey latestBranch.Current.Sha do
            branches.Remove latestBranch |> ignore
            latestBranch <- branches |> Seq.maxBy (fun branch -> branch.Current.Committer.When)

        // Temporarily save as move next changes current
        let current = latestBranch.Current

        // If we can't advance then remove this branch as it has ended
        if not <| latestBranch.MoveNext() then
            branches.Remove latestBranch |> ignore

        current


    let createDefault (commit: LibGit2Sharp.Commit) =
        { parents = []
          children = []
          date = commit.Committer.When
          id = commit.Sha
          messageShort = commit.MessageShort }

    seq {

        while true do
            let currentCommit = getLatestCommit ()

            let current = createDefault currentCommit

            // Try get our children
            let children = childrenByParent.GetValueOrDefault(currentCommit, [])
            // Clear because yield this commit only once and need to get the children only once
            childrenByParent.Remove currentCommit |> ignore

            current.children <- current.children @ children

            // Try get parents from yielded ones or create new
            for parent in currentCommit.Parents do
                let parentCommit =
                    if yieldedById.ContainsKey parent.Sha then
                        yieldedById[parent.Sha]
                    else
                        createDefault parent

                current.parents <- current.parents @ [ parentCommit ]

                // Add current commit to children of commits that still have to come
                if childrenByParent.ContainsKey parent then
                    childrenByParent[parent] <- childrenByParent[parent] @ [ current ]
                else
                    childrenByParent.Add(parent, [ current ])


            yield current
            yieldedById.Add(current.id, current)
    }


let toArray2d coordinates =
    let maxX, _, _ = coordinates |> Array.maxBy (fun (x, _, _) -> x)
    let _, maxY, _ = coordinates |> Array.maxBy (fun (_, y, _) -> y)
    let grid = Array2D.create (maxX + 1) (maxY + 1) None

    for x, y, commit in coordinates do
        Array2D.set grid x y (Some commit)
