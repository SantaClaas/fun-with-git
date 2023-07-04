[<VerifyXunit.UsesVerify>]
module tests.Regression


open System
open System.Collections.Generic
open System.IO
open System.Threading
open System.Threading.Tasks
open Argon
open FunWithGit
open FunWithGit.CommitGraph
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

    let createCommit number parents =
        let commit =
            { id = $"commit %i{number} id"
              messageShort = $"Commit %i{number}"
              date = DateTimeOffset.Now + TimeSpan.FromMinutes(float number)
              parents = parents
              children = [] }

        // Add this commit as child to the parents
        for parent in parents do
            parent.children <- commit :: parent.children

        commit
    
    let result =
        seq {
            (*
            This should represent a graph like this:
            █       Commit 8
            │█      Commit 7
            ││█     Commit 6
            │││█    Commit 5
            ││││█   Commit 4
            █┴┴╯│   Commit 3
            ╰───█   Commit 2
                █   Commit 1
            *)
            let commit1 = createCommit 1 []
            let commit2 = createCommit 2 [ commit1 ]
            let commit3 = createCommit 3 [ commit2 ]
            let commit4 = createCommit 4 [ commit2 ]
            let commit5 = createCommit 5 [ commit3 ]
            let commit6 = createCommit 6 [ commit3 ]
            let commit7 = createCommit 7 [ commit3 ]
            let commit8 = createCommit 8 [ commit3 ]

            // Commits appear in reverse order on the graph
            yield commit8
            yield commit7
            yield commit6
            yield commit5
            yield commit4
            yield commit3
            yield commit2
            yield commit1
        }
        |> List.ofSeq
        // Act
        |> Gitamine.computePositions
        |> Gitamine.drawAsString

    // Assert
    Verifier.Verify(result).ToTask()
