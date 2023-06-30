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
        mutable parents: Commit List
        mutable children: Commit List
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



let sortTemporalTopological (commits: Commit array) =
    let mutable ordered = List()
    let exploredCommits = HashSet()

    let rec depthFirstSearch (commit: Commit) =
        if commit |> exploredCommits.Contains |> not then
            exploredCommits.Add commit |> ignore

            for descendant in commit.children do
                depthFirstSearch descendant

            ordered.Add commit


    for commit in commits |> Seq.sortByDescending (fun commit -> commit.date) do
        depthFirstSearch commit

    ordered

/// <summary>
/// Is <paramref name="b"/> a branch child of <paramref name="a"/>
/// </summary>
let isBranchChildOf (a: Commit) (b: Commit) =
    b.parents.Count > 0 && b.parents[0].id = a.id

/// <summary>
/// Is <paramref name="b"/> a merge child of <paramref name="a"/>
/// </summary>
let isMergeChildOf (a: Commit) (b: Commit) =
    b.parents.Count > 0 && b.parents[0].id <> a.id

let branchChildrenOf (commit: Commit) =
    commit.children |> Seq.filter (isBranchChildOf commit) |> Seq.toArray

let mergeChildrenOf (commit: Commit) =
    commit.children |> Seq.filter (isMergeChildOf commit) |> Seq.toArray

let curvedBranches (orderedCommits: Commit List) =

    seq {
        let branches = List()

        for y = 0 to orderedCommits.Count - 1 do
            let commit = orderedCommits[y]
            let branchChildren = branchChildrenOf commit

            let x =
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

            yield x, y, commit
    }


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

    let childrenByParent: Dictionary<LibGit2Sharp.Commit, Commit List> = Dictionary()
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
        { parents = List()
          children = List()
          date = commit.Committer.When
          id = commit.Sha
          messageShort = commit.MessageShort }

    seq {

        while true do
            let currentCommit = getLatestCommit ()

            let current = createDefault currentCommit

            // Try get our children
            let children = childrenByParent.GetValueOrDefault(currentCommit, List())
            // Clear because yield this commit only once and need to get the children only once
            childrenByParent.Remove currentCommit |> ignore
            current.children.AddRange children

            // Try get parents from yielded ones or create new
            for parent in currentCommit.Parents do
                let parentCommit =
                    if yieldedById.ContainsKey parent.Sha then
                        yieldedById[parent.Sha]
                    else
                        createDefault parent

                current.parents.Add parentCommit

                // Add current commit to children of commits that still have to come
                if childrenByParent.ContainsKey parent then
                    childrenByParent[parent].Add(current)
                else
                    childrenByParent.Add(parent, List([| current |]))


            yield current
            yieldedById.Add(current.id, current)
    }
