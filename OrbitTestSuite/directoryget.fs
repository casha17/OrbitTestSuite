namespace OrbitTestSuite.directoryget
open FSharp.Json
open Hopac
open HttpFs.Client
open FsCheck.Experimental
open FsCheck
open System
open System.IO

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


    let fileupload content =
            Request.createUrl Post "http://localhost:8085/file/upload?userId=100&id=7&version=10&timestamp=638480359110000000"
            |> Request.bodyString content
            |> HttpFs.Client.getResponse 
            |> run    
            |> Response.readBodyAsString
            |> run 


    let file = 
        Request.createUrl Get "http://localhost:8085/file?userId=100&id=7"
        |> Request.responseAsString
        |> run


    let appendToString original next = 
        original + next + ""


    let getString = 
        Arb.generate<string> 
 
    
    type directoryVersion = {
        id: int
        version: int
    }

    type file = {
        id: int
        name: string
        parentId: int
        version: int
        versionChanged: int
        timestamp: string
    }


    type fileList = {
        directoryVersions: list<directoryVersion>
        files : list<file>
        file: string
    }
    let chooseFromList (fileList:fileList) = 
        gen { let! i = Arb.generate<fileList>
        return i }

    type Counter(?initial:fileList) =
        member __.uploadFile(content) = fileupload content
        member __.getFile() = file
        override __.ToString() = sprintf "Counter = %A" "dd"

    let testSuite =
        let uploadFile content = 
            { new Operation<Counter,fileList>() with
                member __.Run m = m.file = content; m
                member __.Check (c,m) = 
                    let res = c.uploadFile content 
                    m.file = res 
                    |@ sprintf "Inc: model = %A, actual = %A" m res
                override __.ToString() = "upload"}
        let getFile = 
            { new Operation<Counter,fileList>() with
                member __.Run m = m.file; m
                member __.Check (c,m) = 
                    let res = c.getFile()
                    m.file = res 
                    |@ sprintf "Dec: model = %A, actual = %A" m res
                override __.ToString() = "getFile"}
        let create initialValue = 
            { new Setup<Counter,fileList>() with
                member __.Actual() = new Counter(initialValue)
                member __.Model() = initialValue }
        { new Machine<Counter,fileList>() with
            member __.Setup = ?? 
            member __.Next _ = Gen.elements [  uploadFile; getFile ] }
    
    (* 
    type HashTable() =
        let mutable file = ""
        member __.Get = file
        member __.fileUpload content = fileupload content
         
        override __.ToString() = sprintf "Counter=%s" file

    let testSuite =
        let FileUpload (content:string option) = { new Command<HashTable,string>() with
                override __.RunActual (model) = model.fileUpload; model
                override __.RunModel m = m
                override __.Post(counter, m) = counter.Get = m |@ sprintf "model: %A <> %A" m  counter.Get
                override __.ToString()  =  sprintf "Gettting...."  }

        { new ICommandGenerator<HashTable,string> with
            member __.InitialActual = HashTable()
            member __.InitialModel = ""
            member __.Next model =  Gen.elements  [ Gen.sized (fun s -> Gen.resize (min s 15) Arb.generate<string>) |> Gen.sample 80 5 |> List.first |>  FileUpload;] }
    
    
    *)