// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open OrbitTestSuite.API
open OrbitTestSuite.Models.Model
open OrbitTestSuite.TestSuite
open OrbitTestSuite.Utilities
open OrbitTestSuite.InMemoryModel
open OrbitTestSuite.Test.test
// Define a function to construct a message to print



[<EntryPoint>]
let main argv =
    // Start testsuite
    testSuite.start
    //let s = API.createFile "100" "15" "test1.txt" "637479675580000000"
    //let v = API.createFile "100" "15" "test1.txt" "637479675580000000"
   // let testData = Utilities.getTestData
   // let s = Utilities.getAllDirId "100"
    //let s = Utilities.fileDeleteModel testData "100" 2 
    //let s = Utilities.getCurrentFileId testData.files
   // printf "%i" s
    //printf "%A" testData.users
    //let s = Utilities.downloadFileModel testData "101" "2" 
    //let v = API.downloadFile "101" "2"
    //let g = match v , s with
    //    | Some g , Some s -> printf "Some: Some"
    //    | None , None -> printf "None: None"
    //let s = Utilities.uploadFileModel testData "xx" "100" "2"
    //match s.content with
    //    |  Some s -> printf "%A" s.users
    (*
    let modelResponse = Utilities.listFilesModel testData "101"
    let sutResponse = Utilities.listFilesSut "101"
    match sutResponse.Fail , sutResponse.Success , modelResponse.Fail , modelResponse.Success with
        | None , Some sut , None , Some model ->  model.fileList = sut.fileList |> printf "%b"
        | Some sut , None , Some model , None ->
            match sut , model with
                | NoUserIdSupplied x , NoUserIdSupplied y  -> printf "NoUserIdSupplied"
        | _ , _ , _ , _ -> printf "EXCEPTION"
    *)
    //let res = API.createFile "100" "15" "test1.dd" "637479675580000000"
    
    let t2 = Branch ("a", [Branch ("b", [Leaf "c"; Leaf "d"]); Branch ("e", [Leaf "f"; Leaf "g"])])

    let rec checkstuff tree =
        match tree with
        | Leaf _ -> true
        | Branch (node, children) ->
        List.fold ( || ) false (List.map checkstuff children)
        
    let s = checkstuff t2 
    
    0 // return an integer exit code