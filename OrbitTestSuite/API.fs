namespace OrbitTestSuite.API

open Hopac
open HttpFs.Client
open OrbitTestSuite.Utilities
open FSharp.Json
open OrbitTestSuite.Models
module API =


    (* 
    type Response<'a> = {
        data: 'a
        statusCode: int
    }
    *)

    type ResponseWithStatusCode = {
        response: Response
        statusCode:int
    } 
    (* 
    let isSuccess (response:Response<'a>) = 
        response.statusCode >= 200 &&  response.statusCode <= 300
    *)
    let createObject (response:Response) = {
        response = Response
        statusCode = R
    }

    let downloadFile userId fileId = 
        Request.createUrl Get (Utilities.concatString ["http://localhost:8085/file?userId="; userId;  "&id=";  fileId]) 
        |> Request.responseAsString
        |> run 
        |> APIModels.fileMapper

    let listFiles userId = 
        Request.createUrl Get (Utilities.concatString ["http://localhost:8085/file/list?userId="; userId;])
        |> HttpFs.Client.getResponse
        |> createObject
        |> run
        |> Response.readBodyAsString
        |> run
        |> Json.deserialize<>





    let fileMetaInformationByFileId userId fileId = 
        Request.createUrl Get (Utilities.concatString ["http://localhost:8085/file/meta?userId="; userId;  "&id=";  fileId]) 
        |> Request.responseAsString
        |> run
        |> Json.deserialize<APIModels.metadata>


    let fileMetaInformationByFileName userId parentId fileName = 
        Request.createUrl Get (Utilities.concatString ["http://localhost:8085/file/meta?userId="; userId;  "&parent_id=";  parentId; "&name="; fileName]) 
        |> Request.responseAsString
        |> run
        |> Json.deserialize<APIModels.metadata>


    let directoryStructure userId = 
        Request.createUrl Get (Utilities.concatString ["http://localhost:8085/dir/structure?userId="; userId;])
        |> Request.responseAsString
        |> run
        |> Json.deserialize<List<APIModels.directoryStructure>>


    let versionCheck userId clientVersion = 
        Request.createUrl Get (Utilities.concatString ["http://localhost:8085/version?userId="; userId;  "&version=";  clientVersion]) 
        |> Request.responseAsString
        |> run

    

    let createFile userId parentId fileName fileTimestamp = 
        Request.createUrl Get (Utilities.concatString ["http://localhost:8085/file?userId="; userId;  "&parent_id=";  parentId; "&name="; fileName; "&timestamp="; fileTimestamp]) 
        |> Request.responseAsString
        |> run
        |> Json.deserialize<APIModels.createFile>

    let fileMove userId fileId fileVersion parentId newFilename = 
        Request.createUrl Get (Utilities.concatString ["http://localhost:8085/file/move?userId="; userId;  "&id=";  fileId; "&version="; fileVersion; "&parentId="; parentId; "&name="; newFilename]) 
        |> Request.responseAsString
        |> run
        |> Json.deserialize<APIModels.moveFile>

    let updateFileTimestamp userId fileId fileVersion fileTimestamp = 
        Request.createUrl Get (Utilities.concatString ["http://localhost:8085/file/timestamp?userId="; userId;  "&id=";  fileId; "&version="; fileVersion; "&timestamp="; fileTimestamp]) 
        |> Request.responseAsString
        |> run
        |> Json.deserialize<APIModels.updateFileTimeStamp>

    let fileupload content userId fileId version timestamp =
        Request.createUrl Post (Utilities.concatString  ["http://localhost:8085/file/upload?userId="; userId; "&id="; fileId; "&version="; version;"&timestamp="; timestamp;])
        |> Request.bodyString content
        |> HttpFs.Client.getResponse 
        |> run    
        
        |> run |> Json.deserialize<APIModels.fileUpload>
    
    type lockState  = Lock | Release

    let intepretLockState lockState = match lockState with 
        | Lock -> "lock" 
        | Release -> "release"
        
    let fileLock userId directoryId fileName lockState = 
        Request.createUrl Get (Utilities.concatString ["http://localhost:8085/file/lock?userId="; userId;  "&parentId=";  directoryId; "&fileName="; fileName; "&state="; (intepretLockState lockState)]) 
        |> Request.responseAsString
        |> run
        |> Json.deserialize<APIModels.fileLock>


    let directoryCreate userId directoryId directoryName directoryVersion = 
        Request.createUrl Get (Utilities.concatString ["http://localhost:8085/dir?userId="; userId;  "&parentId=";  directoryId; "&name="; directoryName; "&version="; directoryVersion]) 
        |> Request.responseAsString
        |> run
        |> Json.deserialize<APIModels.directoryCreate>

    let directoryMove userId directoryId directoryVersion directoryName parentDirectoryId parentDirectoryVersion = 
        Request.createUrl Get (Utilities.concatString ["http://localhost:8085/dir/move?userId="; userId;  "&id=";  directoryId; "&version="; directoryName; "&name="; directoryVersion; "&parentId="; parentDirectoryId; "&parentVersion="; parentDirectoryVersion]) 
        |> Request.responseAsString
        |> run
        |> Json.deserialize<APIModels.directoryMove>

    let fileDelete userId fileId fileVersion = 
        Request.createUrl Get (Utilities.concatString ["http://localhost:8085/file?userId="; userId;  "&id=";  fileId; "&version="; fileVersion]) 
        |> Request.responseAsString
        |> run
        |> Json.deserialize<APIModels.fileDelete>


    let directoryDelete userId directoryId directoryVersion = 
        Request.createUrl Get (Utilities.concatString ["http://localhost:8085/dir?userId="; userId;  "&id=";  directoryId; "&version="; directoryVersion]) 
        |> Request.responseAsString
        |> run
        |> Json.deserialize<APIModels.directoryDelete>
