[<VerifyXunit.UsesVerify>]
module tests.Regression


open System
open System.IO
open System.Threading
open System.Threading.Tasks
open Argon
open FunWithGit
open LibGit2Sharp
open Microsoft.FSharp.Core
open Xunit
open FsUnit.Xunit
open FsUnit.CustomMatchers
open Tests
open VerifyXunit
open VerifyTests

VerifierSettings.AddExtraSettings(fun settings -> settings.NullValueHandling <- NullValueHandling.Include)

[<Fact>]
let ``Can draw Graph 1`` () =
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
            >> Gitamine.drawAsString
        )

    // Assert
    match createResult with
    | Ok resultValue ->  Verifier.Verify(resultValue).ToTask() 
    | Error errorValue -> failwith "todo"