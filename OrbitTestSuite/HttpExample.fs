namespace OrbitTestSuite.HttpExample

open System.Net
open Hopac
open HttpFs.Client
open FSharp.Json
open OrbitTestSuite.Utilities

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
        (Utilties.GetRequests "/dir/structure" "200")
        |> run
        
    printf "%A" bodyContent
   // let getContent = Json.deserialize<list<RecordType>> bodyContent
    
    