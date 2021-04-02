namespace OrbitTestSuite.directoryget
open FSharp.Json
open Hopac
open HttpFs.Client

module directoryget =
    
    type info = {
        id: int 
        parentId: int option
        name: string
        rootPath: string
        rootId: int
        rootIsDefault: bool
        version: int
    }
    
    type files = {
        infos : info list
    }
    
    let initalValue =
       [{
        id = 15
        parentId = None
        name = ""
        rootPath =  "/Users/rw/"
        rootId = 15
        rootIsDefault = true
        version = 1
    }; 
    {
        id = 17
        parentId = None
        name = ""
        rootPath =  "/Projects/Project 1/"
        rootId = 17
        rootIsDefault = false
        version = 1
    };
    
    {
        id = 18
        parentId = None
        name = ""
        rootPath =  "/Projects/Project 2/"
        rootId = 18
        rootIsDefault = false
        version = 1
    }; 
    
    ]
    
    let bodyContent =
        Request.createUrl Get "http://localhost:8085/dir/structure?userId=100"
        |> Request.responseAsString
        |> run
        |> Json.deserialize<list<info>>
    
    
    
    