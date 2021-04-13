namespace OrbitTestSuite.directoryget
open FSharp.Json
open Hopac
open HttpFs.Client

open System
open System.IO

module directoryget =

    type file = {
        content: string
        id: int
        version: int
    } 
    
    type info = {
        id: int 
        parentId: int option
        name: string
        rootPath: string
        rootId: int
        rootIsDefault: bool
        version: int
    }

    type model = {
        files : info list
        statusCode: int
    }
    
    let initalValue = {
        statusCode = 2
        files = 
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
    }

    let createModel (infos:list<info>) = 
        let model = {
            statusCode = 2
            files = infos
        }
        model

    
    let bodyContent =
        Request.createUrl Get "http://localhost:8085/dir/structure?userId=100"
        |> Request.responseAsString
        |> run |> Json.deserialize<list<info>> |> createModel


    let fileupload content versionId =
            Request.createUrl Post "http://localhost:8085/file/upload?userId=100&id=2&version=3&timestamp=638480359110000000"
            |> Request.bodyString content
            |> HttpFs.Client.getResponse 
            |> run    
            |> Response.readBodyAsString
            |> run 


    let getFile = 
        Request.createUrl Get "http://localhost:8085/file?userId=100&id=2"
        |> Request.responseAsString
        |> run


    let appendToString original next = 
        original + next + ""

    
    


