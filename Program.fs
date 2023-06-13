open System.IO
open LibGit2Sharp
open System


let identity = Identity("Claas", "claas.moehlmann@outlook.com")
let author = new Signature(identity, System.DateTimeOffset.Now)



let testRepositoryPath = "./test-repository/"
// Clean previous test repository
if Directory.Exists(testRepositoryPath) then
    Directory.Delete(testRepositoryPath, true)

let createTestRepository path =
    // Create testing repository
    let dotGitPath = Repository.Init(path)

    printfn "Created repository @ %s" dotGitPath

    let isValid = Repository.IsValid(dotGitPath)

    printfn "Repository is valid: %b" isValid
    path

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

let checkoutBranch branchName (repository: Repository) =
    match repository.Branches.Item(branchName) with
    | null -> Error(BranchNotFund(branchName))
    | branch ->
        Commands.Checkout(repository, branch) |> ignore
        Ok repository


let merge signature branchName (repository: Repository) =
    match repository.Branches.Item(branchName) with
    | null -> Error(BranchNotFund(branchName))
    | branch ->
        repository.Merge(branch, signature) |> ignore
        Ok repository


let seedTestRepository () =
    let path = createTestRepository testRepositoryPath
    use repository = new Repository(testRepositoryPath)
    let signature = repository.Config.BuildSignature(DateTimeOffset.Now)

    repository
    |> createCommit signature path 1
    |> createCommit signature path 2
    |> createBranch 1
    |> createCommit signature path 3
    |> checkoutBranch "branch/1"
    |> Result.map (createCommit signature path 4)
    |> Result.bind (checkoutBranch "main")
    |> Result.map (createCommit signature path 5)
    |> Result.bind (merge signature "branch/1")
    |> ignore

    repository.Branches
    |> Seq.iter (fun branch ->
        branch.Commits
        |> Seq.iter (fun commit -> printfn "Branch: %s Commit: %s" branch.FriendlyName commit.Message))

seedTestRepository ()
// |> Result.map (fun repository -> repository.Branches |> Seq.map (fun branch -> branch.FriendlyName))
// |> Result.map (printfn "%A")
