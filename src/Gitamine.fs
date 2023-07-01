module FunWithGit.Gitamine

open System.Collections.Generic
open FunWithGit.CommitGraph
open LibGit2Sharp

type Position = { i: int; j: int }

type Node =
    | Stash of Position
    | Commit of Position

type Edge =
    | Normal of start: Position * end': Position
    | Merge of start: Position * end': Position

/// <summary>
/// Computes forbidden j-coordinates for commit c
/// </summary>
let computeForbiddenIndices c (activeNodes: Map<string, Set<int>>) =
    // Find forbidden indices for highest child
    match c.mergeChildren with
    | [] -> None
    | [ mergeChild ] ->
        // Highest child is the only child so i min is of that child
        activeNodes |> Map.tryFind mergeChild.id
    | mergeChildren ->
        let highestChild = mergeChildren |> List.minBy (fun d -> d.i)
        activeNodes |> Map.tryFind highestChild.id
    |> Option.defaultValue Set.empty

// List helper to "replace at". Very inefficient
let replaceAt index item list =
    list |> List.mapi (fun index' item' -> if index' = index then item else item')

/// <summary>
/// Loops through all elements and adds them to a set
/// </summary>
let rec addAll items set =
    match items with
    | [] -> set
    | [ item ] -> set |> Set.add item
    | head :: tail -> set |> addAll tail |> Set.add head

// Because I read 👊
let first = fst

let insertCommit commit j forbiddenIndices branches =

    // Try to insert as close as possible to i
    // replace i by j
    // We try to find the free index close to j as the "middle"
    // dj is delta to j and it increases each iteration to find the first free branch index right or left of j
    let mutable dj = 1

    let isAllowed index =
        forbiddenIndices |> Set.contains index |> not

    let isNone index =
        branches |> List.item index |> Option.isNone

    let rec findInsertIndex startJ deltaJ branches =
        let left, right = startJ - deltaJ, startJ + deltaJ
        let length = List.length branches
        // Base exit condition: When we could not find a free index left and right from the start index
        if left < 0 && right >= length then
            // If it is not possible to find an available position, append
            None
        elif right < length && isAllowed right && isNone right then
            Some right
        elif left >= 0 && isAllowed left && isNone left then
            Some left
        else
            // Continue search and advance delta
            findInsertIndex startJ (deltaJ + 1) branches

    match branches |> findInsertIndex j 1 with
    | Some index -> branches |> replaceAt index (Some commit), index
    | None ->
        // "Add" to end
        let newBranches = branches @ [ Some commit ]
        newBranches, List.length newBranches - 1

let computePositions (crommits: Crommit list) =
    let mutable positions = Map.empty

    //let headSha = (repository |> List.head).id
    let mutable i = 1
    let mutable branches = []
    let mutable activeNodes = Map.empty
    let activeNodesQueue = PriorityQueue()
    activeNodes <- activeNodes |> Map.add "index" Set.empty

    for commit in crommits do
        let mutable j = -1
        let commitSha = commit.id

        // Compute forbidden indices
        let forbiddenIndices = computeForbiddenIndices commit activeNodes

        // Find a commit to replace
        // The commit can only replace a child whose first parent is this commit
        let commitToReplace =
            commit.branchChildren
            |> List.map (fun child -> positions |> Map.find child.id |> first, child)
            |> List.filter (fun (jChild, _) -> forbiddenIndices |> Set.contains jChild |> not)
            // Min by j or none
            |> (function
            | [] -> None
            | [ child ] -> Some child
            | children -> children |> List.minBy first |> Some)

        // Insert the commit in the active branches
        let newBranches, j =
            match commitToReplace with
            | Some(j, commitToReplace) -> branches |> replaceAt j (Some commitToReplace), j
            | None -> branches |> insertCommit commit 0 Set.empty

        branches <- newBranches

        // Remove useless active nodes
        while activeNodesQueue.Count > 0 && activeNodesQueue.Peek() |> first < i do
            let commit = activeNodesQueue.Dequeue() |> snd
            activeNodes <- activeNodes |> Map.remove commit.id

        // Update the active nodes
        let jToAdd =
            j
            :: (commit.branchChildren
                |> List.map (fun child -> positions |> Map.find child.id |> snd))

        // Add all j to active nodes
        activeNodes <-
            activeNodes
            |> Map.map (fun _ indices -> indices |> addAll jToAdd)
            |> Map.add commit.id Set.empty
        // (Get the highest i of the parents add it to the queue to be removed later?)
        // Get the parent when we want to remove the active nodes?
        let iRemove = commit.parents |> List.map (fun parent -> parent.i) |> List.max
        activeNodesQueue.Enqueue((iRemove, commit), iRemove)

        //TODO is commit to replace only existent when we come from a branching
        // Remove children from active branches
        match commitToReplace with
        | Some(j, commitToReplace) ->
            // Get j positions of children
            let childJsToDeleteFromBranches =
                commit.branchChildren
                // Get children that are not the commit to replace
                |> List.filter (fun child -> child.id <> commitToReplace.id)
                // Get j of those and set that position in branch to none
                |> List.map (fun child -> positions |> Map.find child.id |> snd)
                |> Set

            // Remove branches that are in child js to remove
            branches <-
                branches
                |> List.map (
                    Option.bind (fun branch ->
                        if childJsToDeleteFromBranches |> Set.contains branch.id then
                            None
                        else
                            Some branch)
                )
        // If commit to replace is none do nothing
        | None -> ()


        // If the commit has no parents, remove it from the active branches
        if commit.parents |> List.isEmpty then
            branches <- branches |> replaceAt j None



        // Finally set the position
        positions <- positions |> Map.add commit.id (i, j, commit)
        i <- i + 1

    let width = List.length branches
    
    ()
