namespace OrbitTestSuite.API

open Hopac
open HttpFs.Client

open FSharp.Json
open OrbitTestSuite.Models
module API =

    let concatString x = 
        String.concat "" x
    
    type BaseResponse<'a> = {
        data: 'a
        response: Response
    }
    
    let isSuccess (response:BaseResponse<'a>) = 
        response.response.statusCode >= 200 &&  response.response.statusCode <= 300
    
    let downloadFile userId fileId = 
        let result = 
            Request.createUrl Get (concatString ["http://localhost:8085/file?userId="; userId;  "&id=";  fileId]) 
            |> getResponse
            |> run 
        
        let data = 
            Response.readBodyAsString result
            |> run
        
        {
            data = data
            response = result
        }
        
    let listFiles userId = 
        let result =
            Request.createUrl Get (concatString ["http://localhost:8085/file/list?userId="; userId;])
            |> getResponse
            |> run
            
        let data =
            Response.readBodyAsString result
            |> run
            |> Json.deserialize<APIModels.listFilesResponse>
        
        {
            data = data
            response = result
        }

    let fileMetaInformationByFileId userId fileId = 
        let result =
            Request.createUrl Get (concatString ["http://localhost:8085/file/meta?userId="; userId;  "&id=";  fileId])
            |> getResponse
            |> run
        
        let data =
            Response.readBodyAsString result
            |> run
            |> Json.deserialize<APIModels.metadata>
           
        {
            data = data
            response = result
        }

    let fileMetaInformationByFileName userId parentId fileName = 
        
        let result =
            Request.createUrl Get (concatString ["http://localhost:8085/file/meta?userId="; userId;  "&parent_id=";  parentId; "&name="; fileName]) 
            |> getResponse
            |> run
        
        let data =
            Response.readBodyAsString result
            |> run
            |> Json.deserialize<APIModels.metadata>

        {
            data = data
            response = result
        }

    let directoryStructure userId = 
        let result =
            Request.createUrl Get (concatString ["http://localhost:8085/dir/structure?userId="; userId;])
            |> getResponse
            |> run
        
        let data =
            Response.readBodyAsString result
            |> run
            |> Json.deserialize<List<APIModels.directoryStructure>>
        {
            data = data
            response = result
        }

    let versionCheck userId clientVersion = 
        
        let result =
            Request.createUrl Get (concatString ["http://localhost:8085/version?userId="; userId;  "&version=";  clientVersion]) 
            |> getResponse
            |> run
        {
            response = result
            data = ()
        }
    
    let createFile userId parentId fileName fileTimestamp = 
        let result =
            Request.createUrl Get (concatString ["http://localhost:8085/file?userId="; userId;  "&parent_id=";  parentId; "&name="; fileName; "&timestamp="; fileTimestamp]) 
            |> getResponse
            |> run
        
        let data =
            Response.readBodyAsString result
            |> run
            |> Json.deserialize<APIModels.createFile>
        {
            response = result
            data = data
        }   

    let fileMove userId fileId fileVersion parentId newFilename = 
        let result =
            Request.createUrl Get (concatString ["http://localhost:8085/file/move?userId="; userId;  "&id=";  fileId; "&version="; fileVersion; "&parentId="; parentId; "&name="; newFilename]) 
            |> getResponse
            |> run
        
        let data =
            Response.readBodyAsString result
            |> run
            |> Json.deserialize<APIModels.moveFile>
        
        {
            response = result
            data = data
        }

    let updateFileTimestamp userId fileId fileVersion fileTimestamp = 
        let result =
            Request.createUrl Get (concatString ["http://localhost:8085/file/timestamp?userId="; userId;  "&id=";  fileId; "&version="; fileVersion; "&timestamp="; fileTimestamp]) 
            |> getResponse
            |> run
        
        let data =
            Response.readBodyAsString result
            |> run
            |> Json.deserialize<APIModels.updateFileTimeStamp>
        
        {
            data = data
            response = result
        }
    
    let fileupload content userId fileId version timestamp =
        let result =
            Request.createUrl Post (concatString  ["http://localhost:8085/file/upload?userId="; userId; "&id="; fileId; "&version="; version;"&timestamp="; timestamp;])
            |> getResponse    
            |> run
        
        let data =
            Response.readBodyAsString result
            |> run
            |> Json.deserialize<APIModels.fileUpload>
        
        {
            data = data
            response = result
        }
    
    type lockState  = Lock | Release

    let intepretLockState lockState = match lockState with 
        | Lock -> "lock" 
        | Release -> "release"
        
    let fileLock userId directoryId fileName lockState = 
        let result =
            Request.createUrl Get (concatString ["http://localhost:8085/file/lock?userId="; userId;  "&parentId=";  directoryId; "&fileName="; fileName; "&state="; (intepretLockState lockState)]) 
            |> getResponse
            |> run
        
        let data =
            Response.readBodyAsString result
            |> run
            |> Json.deserialize<APIModels.fileLock>
        
        {
            data = data
            response = result
        }

    let directoryCreate userId directoryId directoryName directoryVersion = 
        let result =
            Request.createUrl Get (concatString ["http://localhost:8085/dir?userId="; userId;  "&parentId=";  directoryId; "&name="; directoryName; "&version="; directoryVersion]) 
            |> getResponse
            |> run
        
        let data =
            Response.readBodyAsString result
            |> run
            |> Json.deserialize<APIModels.directoryCreate>
        
        {
            data = data
            response = result
        }

    let directoryMove userId directoryId directoryVersion directoryName parentDirectoryId parentDirectoryVersion = 
        let result =
            Request.createUrl Get (concatString ["http://localhost:8085/dir/move?userId="; userId;  "&id=";  directoryId; "&version="; directoryName; "&name="; directoryVersion; "&parentId="; parentDirectoryId; "&parentVersion="; parentDirectoryVersion]) 
            |> getResponse
            |> run
        
        let data =
            Response.readBodyAsString result
            |> run
            |> Json.deserialize<APIModels.directoryMove>
        
        {
            data = data
            response = result
        }

    let fileDelete userId fileId fileVersion = 
        let result =
            Request.createUrl Get (concatString ["http://localhost:8085/file?userId="; userId;  "&id=";  fileId; "&version="; fileVersion]) 
            |> getResponse
            |> run
        
        let data =
            Response.readBodyAsString result
            |> run
            |> Json.deserialize<APIModels.fileDelete>

        {
            data = data
            response = result
        }
    
    let directoryDelete userId directoryId directoryVersion = 
        let result =
            Request.createUrl Get (concatString ["http://localhost:8085/dir?userId="; userId;  "&id=";  directoryId; "&version="; directoryVersion]) 
            |> getResponse
            |> run
        
        let data =
            Response.readBodyAsString result
            |> run
            |> Json.deserialize<APIModels.directoryDelete>
        
        {
            data = data
            response = result
        }
