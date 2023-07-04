/// <summary>
/// These tests create files and are depending on implementations so are they end-to-end tests or integration tests?
/// </summary>
module Tests

open System
open System.IO
open System.Threading
open FunWithGit
open LibGit2Sharp
open Microsoft.FSharp.Core
open Xunit
open FsUnit.Xunit
open FsUnit.CustomMatchers


[<Literal>]
let main = "main"

let identity = Identity("Test Identity", "claas.moehlmann@outlook.com")
let author () = Signature(identity, DateTimeOffset.Now)

let createCommit signature path commitNumber (repository: Repository) =
    let fileName = $"commit {commitNumber}.txt"
    let filePath = path + fileName
    File.WriteAllText(filePath, $"Hello commit {commitNumber}!")

    repository.Index.Add(fileName)
    repository.Index.Write()
    repository.Commit($"Commit {commitNumber}", signature, signature) |> ignore
    repository

let createBranch branchNumber (repository: Repository) =
    repository.CreateBranch($"branch/{branchNumber}") |> ignore
    repository

type CheckoutError = BranchNotFund of string

let checkoutBranchName name (repository: Repository) =
    match repository.Branches.Item(name) with
    | null -> Error(BranchNotFund(name))
    | branch ->
        Commands.Checkout(repository, branch) |> ignore
        Ok repository


let checkoutBranch branchNumber =
    checkoutBranchName $"branch/{branchNumber}"

let merge signature branchName (repository: Repository) =
    match repository.Branches.Item(branchName) with
    | null -> Error(BranchNotFund(branchName))
    | branch ->
        repository.Merge(branch, signature) |> ignore
        Ok repository

[<Fact>]
let ``Main branch continues in column 4 of "branch/1" after "Commit 3"`` () =
    // Arrange
    // Create temp repository
    let expectedCoordinates =
        [| (0, 0); (1, 1); (2, 2); (3, 3); (4, 4); (5, 0); (6, 4); (7, 4) |] |> List.ofArray

    let path = $"./temp/tests/test-repository/7B85F3C6-D634-486B-96EA-8FC90B06A4BE/"

    if Directory.Exists path then
        Directory.Delete(path, true)

    // Act
    Repository.Init(path) |> ignore
    use repository = new Repository(path)
    // Need to sufficiently space out commits or it will cause issues with sorting as time is taken into account
    let mutable counter = 0

    let signature () =
        counter <- counter + 1
        repository.Config.BuildSignature(DateTimeOffset.Now + TimeSpan.FromMinutes counter)

    let createCommit' number = createCommit (signature ()) path number


    let createResult =
        repository
        // Commit on main
        |> createCommit' 1
        // Commit on main
        |> createCommit' 2
        // Create branch
        |> createBranch 1
        // Switch back to main and commit
        |> checkoutBranchName main
        |> Result.bind (
            createCommit' 3
            // Back to branch 1 and create commit
            >> checkoutBranch 1
        )
        |> Result.bind (createCommit' 4 >> checkoutBranchName main)
        |> Result.bind (createBranch 2 >> checkoutBranch 2)
        |> Result.bind (createCommit' 5 >> checkoutBranchName main)
        |> Result.bind (createBranch 3 >> checkoutBranch 3)
        |> Result.bind (createCommit' 6 >> checkoutBranchName main)
        |> Result.bind (createBranch 4 >> checkoutBranch 4)
        |> Result.bind (createCommit' 7 >> checkoutBranchName main)
        |> Result.bind (createBranch 5 >> checkoutBranch 5)
        |> Result.bind (createCommit' 8 >> checkoutBranchName main)
        |> Result.map (
            CommitGraph.listCommits
            >> Seq.take 8
            >> Seq.toArray
            >> CommitGraph.sortTemporalTopological
            >> CommitGraph.curvedBranches
            >> Seq.map (fun (x, y, _) -> x, y)
            >> Seq.toArray
        )

    // Assert
    createResult |> should be (ofCase <@ Result<(int * int) array, CheckoutError>.Ok @>)
    
    // Can only be ok here, not sure how to extract result better
    match createResult with
    | Ok results -> results |> List.ofArray |> should matchList expectedCoordinates
    | Error _ -> failwith "Result should be Ok but was Error"



[<Fact>]
let ``(Gitamine) Main branch continues in column 4 of "branch/1" after "Commit 3"`` () =
    // Arrange
    // Create temp repository
    let expectedCoordinates =
        [| (0, 0); (1, 1); (2, 2); (3, 3); (4, 4); (5, 0); (6, 4); (7, 4) |] |> List.ofArray

    let path = $"./temp/tests/test-repository/7B85F3C6-D634-486B-96EA-8FC90B06A4BE/"

    if Directory.Exists path then
        Directory.Delete(path, true)

    // Act
    Repository.Init(path) |> ignore
    use repository = new Repository(path)
    // Need to sufficiently space out commits or it will cause issues with sorting as time is taken into account
    let mutable counter = 0

    let signature () =
        counter <- counter + 1
        repository.Config.BuildSignature(DateTimeOffset.Now + TimeSpan.FromMinutes counter)

    let createCommit' number = createCommit (signature ()) path number


    let createResult =
        repository
        // Commit on main
        |> createCommit' 1
        // Commit on main
        |> createCommit' 2
        // Create branch
        |> createBranch 1
        // Switch back to main and commit
        |> checkoutBranchName main
        |> Result.bind (
            createCommit' 3
            // Back to branch 1 and create commit
            >> checkoutBranch 1
        )
        |> Result.bind (createCommit' 4 >> checkoutBranchName main)
        |> Result.bind (createBranch 2 >> checkoutBranch 2)
        |> Result.bind (createCommit' 5 >> checkoutBranchName main)
        |> Result.bind (createBranch 3 >> checkoutBranch 3)
        |> Result.bind (createCommit' 6 >> checkoutBranchName main)
        |> Result.bind (createBranch 4 >> checkoutBranch 4)
        |> Result.bind (createCommit' 7 >> checkoutBranchName main)
        |> Result.bind (createBranch 5 >> checkoutBranch 5)
        |> Result.bind (createCommit' 8 >> checkoutBranchName main)
        |> Result.map (
            CommitGraph.listCommits
            >> Seq.take 8
            >> Seq.toArray
            >> CommitGraph.sortTemporalTopological
            >> Gitamine.computePositions
        )

    // Assert
    createResult |> should be (ofCase <@ Result<(int * int) array, CheckoutError>.Ok @>)
    
    // Can only be ok here, not sure how to extract result better
    match createResult with
    | Ok results -> results |> List.ofArray |> should matchList expectedCoordinates
    | Error _ -> failwith "Result should be Ok but was Error"


