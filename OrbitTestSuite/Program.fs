// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp


open FSharp.Json
open HttpFs.Client
open Hopac
open OrbitTestSuite.HttpExample
open OrbitTestSuite.TestSuite
open OrbitTestSuite.directoryget
open FsCheck
// Define a function to construct a message to print



[<EntryPoint>]
let main argv =

    //testSuite.setupTestSuite
    //let check e =  List.isEmpty e  = List.isEmpty Http.getContent
    

    //let rec check s  = match s with
    //| [] -> printf "empty"
    //| el::els -> check els; printf "\n\n  %A " el
    
    //printf "%A" Http.bodyContent
    //printf "%A" Http.getContent
   
    testSuite.setupTestSuite
    (* let fileupload content =
           Request.createUrl Post "http://localhost:8085/file/upload?userId=100&id=7&version=8&timestamp=638480359110000000"
            |> Request.bodyString content
            |> HttpFs.Client.getResponse 
            |> run    
            |> Response.readBodyAsString
            |> run 

    printf "%s" (fileupload "fmmg")

    *)
    0 // return an integer exit code