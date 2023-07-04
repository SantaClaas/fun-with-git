module FunWithGit.Gitamine

open System
open System.Buffers
open System.Collections.Generic
open System.Text
open FunWithGit.CommitGraph
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Core

type Position = Position of i: int * j: int

type Node =
    | Stash of Position
    | Commit of Position

type Edge =
    | Normal of start: Position * end': Position
    | Merge of start: Position * end': Position

/// <summary>
/// Computes forbidden j-coordinates for commit c
/// </summary>
let computeForbiddenIndices commit activeNodes indexOf =
    let mergeChildren = mergeChildrenOf commit
    // Find forbidden indices for highest child
    match mergeChildren with
    | [||] -> None
    | [| mergeChild |] ->
        // Highest child is the only child so i min is of that child
        activeNodes |> Map.tryFind mergeChild.id
    | mergeChildren ->
        let highestChild = mergeChildren |> Array.minBy indexOf
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

let computePositions (commits: Commit list) =
    let indicesById =
        commits |> List.mapi (fun index commit -> commit.id, index) |> Map.ofList

    let indexOf commit = indicesById |> Map.find commit.id

    let mutable positions = Map.empty
    let mutable result = commits |> List.length |> Array.zeroCreate
    let positionOf child = positions |> Map.find child.id
    let jPositionOf = positionOf >> snd

    //let headSha = (repository |> List.head).id
    let mutable i = 0
    let mutable branches = []
    let mutable activeNodes = Map.empty
    let activeNodesRemoveQueue = PriorityQueue()

    for commit in commits do

        let branchChildren = branchChildrenOf commit
        // Compute forbidden indices
        let forbiddenIndices = computeForbiddenIndices commit activeNodes indexOf

        // Find a commit to replace
        // The commit can only replace a child whose first parent is this commit
        let commitToReplace =
            branchChildren
            |> Array.map (fun child -> positionOf child, child)
            |> Array.filter (fun ((_, jChild), _) -> forbiddenIndices |> Set.contains jChild |> not)
            // Min by j or none
            |> (function
            | [||] -> None
            | [| child |] -> Some child
            // There are two strategies we can choose from
            // 1. Place commit as far left as possible by taking the min of j
            // | children -> children |> Array.minBy (fst >> snd) |> Some)
            // 2. Continue the branch that first said this commit is it's parent by taking the min of i
            | children -> children |> Array.minBy (fst >> fst) |> Some)


        // Insert the commit in the active branches
        let newBranches, j =
            match commitToReplace with
            | Some((i, j), commitToReplace) -> branches |> replaceAt j (Some commitToReplace), j
            | None -> branches |> insertCommit commit 0 Set.empty

        branches <- newBranches

        // Remove useless active nodes
        while activeNodesRemoveQueue.Count > 0 && activeNodesRemoveQueue.Peek() |> first < i do
            let commit = activeNodesRemoveQueue.Dequeue() |> snd
            activeNodes <- activeNodes |> Map.remove commit.id
            ()

        // Update the active nodes
        let jToAdd = j :: (branchChildren |> Array.map jPositionOf |> List.ofArray)

        // Add all j to active nodes
        activeNodes <-
            activeNodes
            |> Map.map (fun _ indices -> indices |> addAll jToAdd)
            |> Map.add commit.id Set.empty

        // (Get the highest i of the parents add it to the queue to be removed later?)
        // Get the parent when we want to remove the active nodes?
        // Get the i for when to remove current commit from active ones. The i is the last parent
        let iRemove =
            match commit.parents with
            //TODO find a better solution to represent -Infinity
            | [] -> Int32.MinValue
            | [ one ] -> indexOf one
            | multiple -> multiple |> List.map indexOf |> List.max

        activeNodesRemoveQueue.Enqueue((iRemove, commit), iRemove)

        //TODO is commit to replace only existent when we come from a branching
        // Remove children from active branches
        match commitToReplace with
        | Some(j, commitToReplace) ->
            // Get j positions of children
            // (like for childSha of branchChildren then remove if not commit to replace)
            let childJsToDeleteFromBranches =
                branchChildren
                // Get children that are not the commit to replace
                |> Array.filter (fun child -> child.id <> commitToReplace.id)
                // Get j of those and set that position in branch to none
                |> Array.map jPositionOf
                |> Set

            // Remove branches that are in child js to remove


            branches <-
                branches
                |> List.mapi (fun j ->
                    Option.bind (fun branch ->
                        if childJsToDeleteFromBranches |> Set.contains j then
                            None
                        else
                            Some branch))
        // If commit to replace is none do nothing
        | None -> ()


        // If the commit has no parents, remove it from the active branches
        if commit.parents |> List.isEmpty then
            branches <- branches |> replaceAt j None




        // Finally set the position
        positions <- positions |> Map.add commit.id (i, j)
        Array.set result i (i, j, commit)
        i <- i + 1

    result

//
(*
  ◯
◯ │   ◯ ─╮
│ │ ◯ │  ◯
╰─◯─╯ ◯


╭────╮─
│    │─╭─╮
╰────╯─╰─╯─
*)

type CurveDrawInstruction =
    | LeftToUp
    | LeftToDown
    | RightToUp
    | RightToDown

type LineDrawInstruction =
    | Horizontal
    | Vertical

type DrawInstruction =
    | Curve of CurveDrawInstruction
    | Line of LineDrawInstruction
    | Commit

let drawAsString positions =

    let positionByCommit =
        positions |> Array.map (fun (i, j, commit) -> commit.id, (i, j)) |> Map.ofArray

    let positionOf commit = positionByCommit |> Map.find commit.id

    let rowCount = Array.length positions

    let rowLength =
        positions
        |> Array.map (fun (_, j, _) -> j)
        |> Array.max
        |> (+) 1
        |> (+) Environment.NewLine.Length

    let grid = Array.create (rowCount * rowLength) ' '
    // let grid = Array2D.create (Array.length positions) (maxJ + 1) " "




    // Set row ends to new line
    let startNewLine = rowLength - Environment.NewLine.Length

    for row = 0 to rowCount - 1 do
        let start = row * rowLength + (startNewLine)
        // let end' = row * rowLength + 1 + Environment.NewLine.Length
        for index = 0 to Environment.NewLine.Length - 1 do
            let character = Environment.NewLine[index]
            grid[start + index] <- character

    let drawAt instruction i j =
       
        let symbol =
            match instruction with
            | Line direction ->
                match direction with
                | Horizontal -> '─'
                | Vertical -> '│'

            | Curve direction ->
                match direction with
                | LeftToUp -> '╯'
                | RightToUp -> '╰'
                | LeftToDown -> '╮'
                | RightToDown -> '╭'
            | Commit -> '█'


        let index = (i * rowLength) + j
        // Replace previous
        if index - 1 >= 0 then
            match grid[index - 1], symbol with
            | '╯', '╯' -> grid[index - 1] <- '┴'
            | '╰', '╰' -> grid[index] <- '┴'
            | '╭', '╭' -> grid[index] <- '┬'
            | '╮', '╮' -> grid[index - 1] <- '┬'
            | _ -> ()
        
        // Don't replace '╮' or other symbols
        if grid[index] = ' ' then
            grid[index] <- symbol

    let drawHorizontal from to' i =
        for j = from to to' do
            drawAt (Horizontal |> Line) i j

    let drawVertical from to' j =
        for i = from to to' do
            drawAt (Vertical |> Line) i j



    for iCommit, jCommit, commit in positions do
        // this (U+2588) or"◯"
        drawAt Commit iCommit jCommit
        let branchChildren = branchChildrenOf commit
        let mergeChildren = mergeChildrenOf commit

        // Draw backwards to children
        for child in branchChildren do
            let iChild, jChild = positionOf child

            // For branch children we draw to the j of the child

            //  "Go right"
            if jCommit < jChild then
                drawHorizontal (jCommit + 1) (jChild - 1) iCommit
                drawAt (LeftToUp |> Curve) iCommit jChild
            // set '╯' iCommit jChild
            // "Go left"
            elif jChild < jCommit then
                drawHorizontal (jChild + 1) (jCommit - 1) iCommit
                drawAt (RightToUp |> Curve) iCommit jChild
            // set '╰' iCommit jChild

            // Draw line
            drawVertical (iChild + 1) (iCommit - 1) jChild

        // The commit bubble should have been drawn

        for child in mergeChildren do
            let iChild, jChild = positionOf child

            //  "Go right"
            if jCommit < jChild then
                drawHorizontal (jCommit + 1) (jChild - 1) iChild
                drawAt (LeftToDown |> Curve) iCommit jChild
            // set '╮' iCommit jChild
            // "Go left"
            elif jChild < jCommit then
                drawHorizontal (jChild + 1) (jCommit - 1) iChild
                drawAt (RightToDown |> Curve) iCommit jChild
            // set '╭' iCommit jChild

            // Draw line
            drawVertical (iChild + 1) (iCommit - 1) jCommit


    String grid
