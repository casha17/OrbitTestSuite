// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open OrbitTestSuite.API
open OrbitTestSuite.Models.Model
open OrbitTestSuite.TestSuite
open OrbitTestSuite.Utilities
open OrbitTestSuite.InMemoryModel

// Define a function to construct a message to print



[<EntryPoint>]
let main argv =
    // Start testsuite
    testSuite.start
    let testData = Utilities.getTestData
    //printf "%A" testData.users
    //let s = Utilities.downloadFileModel testData "101" "2" 
    //let v = API.downloadFile "101" "2"
    //let g = match v , s with
    //    | Some g , Some s -> printf "Some: Some"
    //    | None , None -> printf "None: None"
    //let s = Utilities.uploadFileModel testData "xx" "100" "2"
    //match s.content with
    //    |  Some s -> printf "%A" s.users
    
    let modelResponse = Utilities.listFilesModel testData "101"
    let sutResponse = Utilities.listFilesSut "101"
    match sutResponse.Fail , sutResponse.Success , modelResponse.Fail , modelResponse.Success with
        | None , Some sut , None , Some model ->  model.fileList = sut.fileList |> printf "%b"
        | Some sut , None , Some model , None ->
            match sut , model with
                | NoUserIdSupplied x , NoUserIdSupplied y  -> printf "NoUserIdSupplied"
        | _ , _ , _ , _ -> printf "EXCEPTION"
            
    0 // return an integer exit code