// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System.Diagnostics
open System.Threading
open Hopac.Core
open Hopac.Extensions
open OrbitTestSuite.API
open OrbitTestSuite.DockerIntegration
open OrbitTestSuite.Models.Model
open OrbitTestSuite.TestSuite
open OrbitTestSuite.Utilities
open OrbitTestSuite.InMemoryModel
open OrbitTestSuite.Test.test
// Define a function to construct a message to print
open Hopac


[<EntryPoint>]
let main argv =
    // Start testsuite
    let r =  Docker.executeShellCommand "docker run -d --name orbit --rm -p8085:8085 -eCLICOLOR_FORCE=2 cr.orbit.dev/sdu/filesync-server:latest"  |> Async.RunSynchronously
    let r =  Docker.executeShellCommand "docker run -d --name orbit1 --rm -p8084:8085 -eCLICOLOR_FORCE=2 cr.orbit.dev/sdu/filesync-server:latest"  |> Async.RunSynchronously
    let r =  Docker.executeShellCommand "docker run -d --name orbit2 --rm -p8083:8085 -eCLICOLOR_FORCE=2 cr.orbit.dev/sdu/filesync-server:latest"  |> Async.RunSynchronously
    let r =  Docker.executeShellCommand "docker run -d --name orbit3 --rm -p8082:8085 -eCLICOLOR_FORCE=2 cr.orbit.dev/sdu/filesync-server:latest"  |> Async.RunSynchronously
    let r =  Docker.executeShellCommand "docker run -d --name orbit4 --rm -p8081:8085 -eCLICOLOR_FORCE=2 cr.orbit.dev/sdu/filesync-server:latest"  |> Async.RunSynchronously
    let r =  Docker.executeShellCommand "docker run -d --name orbit5 --rm -p8080:8085 -eCLICOLOR_FORCE=2 cr.orbit.dev/sdu/filesync-server:latest"  |> Async.RunSynchronously

    //Thread.Sleep 500
    testSuite.start
    

    //let testData = Utilities.getTestData
    //let s = Utilities.createFileModel testData "100" 15 "test1.txt"
    //let g = Utilities.uploadFileModel testData "c" "100" 4  28 "637479675580000000"
   // printf "s"
    //let s = API.createFile "100" "15" "test1.txt" "637479675580000000"
    //let v = API.createFile "100" "15" "test1.txt" "637479675580000000"
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
    
    
    //let r =  Docker.executeShellCommand "docker stop" |> Async.RunSynchronously

    0 // return an integer exit code