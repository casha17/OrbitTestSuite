﻿// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System.Diagnostics
open System.Threading
open FsCheck
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
    testSuite.start
    let testData = Utilities.getTestData
    //let s = API.directoryStructure "100"
    (*
    let testData = Utilities.getTestData
    let xxx = Utilities.createDirectoryModel testData "100" 15 "test1.txt"  1
    let y = Utilities.createFileModel xxx.Success.Value "100" 15 "test2.txt"
    let a = Utilities.createFileModel y.Success.Value "100" 22 "test1.txt"
    let b = Utilities.updateFileTimestampModel a.Success.Value "100" 6 1 "637479675580000000" 
    let c = Utilities.updateFileTimestampModel b.Success.Value "100" 6 2 "637479675580000000" 
    let d = Utilities.updateFileTimestampModel c.Success.Value "100" 6 3 "637479675580000000" 
    let e = Utilities.updateFileTimestampModel d.Success.Value "100" 6 4 "637479675580000000" 
    let f = Utilities.moveFileModel e.Success.Value "100" 6 15 "test1.txt" 5
    let g = Utilities.listFilesModel e.Success.Value "100"
  //  let a = Utilities.moveFileModel  uu.Success.Value "100" 2 15 "test2.txt" 2
   // let b = Utilities.fileDeleteModel testData "9999" 2 34
    printf "s"
     (*
    let c = Utilities.fileDeleteModel testData "100" 2 1
    let d = Utilities.fileDeleteModel c.Success.Value "100" 4 1
    let e = Utilities.moveFileModel testData "100" 3 16 "test1.txt" 1
    let f = Utilities.moveFileModel testData "100" 3 15 "test1.txt" 1

    let g = Utilities.createFileModel testData "100" 15 "test2.txt" //Success
    let u = Utilities.moveFileModel g.Success.Value  "100" 5 20 "test2.txt" 1
   // let f = Utilities.moveFileModel u.Success.Value  "100" 6 15 "test2.txt" 1 // conflict
    //let c = Utilities.createFileModel f.Success.Value "100" 8 "test2.txt" // success
    //let a = Utilities.createFileModel {f.Success.Value with currentFileId = 8} "100" 15 "test2.txt" // conflict
    //let qq = Utilities.moveFileModel a.Success.Value  "100" 8 15 "test2.txt" 1 // success
  //  let u = Utilities.createFileModel qq.Success.Value "100" 20 "test2.txt"
   // let ux = Utilities.createFileModel testData  "100" 15 "test1.txt" 
   // let ux = Utilities.moveFileModel ux.Success.Value  "100" 5 15 "test1.txt" 1
    printf "s"
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
    //let r =  Docker.executeShellCommand "docker stop orbit"  |> Async.RunSynchronously
    *)
    *)
  
    let mutable fileIds = Utilities.getAllFileIds testData.files
    let gen = Gen.frequency [(2 ,Gen.elements fileIds); (1 ,Arb.generate<int>); (1 , Gen.choose(-1,1))]
    let result = gen.Sample (2,100)
    printf "%A" result

    0 // return an integer exit code
