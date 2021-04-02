namespace OrbitTestSuite.HttpExample

open System.Net
open Hopac
open HttpFs.Client
open FSharp.Json

module Http =
    
    type parentId = 
        | ParentId of int option
    
    type RecordType = {
        id: int 
        parentId: parentId
        name: string
        rootPath: string
        rootId: int
        rootIsDefault: bool
        version: int
    }


        
    let bodyContent =
        Request.createUrl Get "http://localhost:8085/dir/structure?userId=100"
        |> Request.responseAsString
        |> run
        
    let getContent = Json.deserialize<list<RecordType>> bodyContent
    