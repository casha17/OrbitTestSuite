namespace OrbitTestSuite.API

open Hopac
open HttpFs.Client

open FSharp.Json
open OrbitTestSuite.Models
open OrbitTestSuite.Models.Model
module API =

    let concatString x = 
        String.concat "" x
    
    type BaseResponse<'a> = {
        data: 'a
        response: HttpFs.Client.Response
        fail: bool
    }
    
    let isSuccess (response:BaseResponse<'a>) = 
        response.response.statusCode >= 200 &&  response.response.statusCode <= 300
    
    let downloadFile userId fileId = 
        let result = 
            Request.createUrl Get (concatString ["http://localhost:8085/file?userId="; userId;  "&id=";  fileId]) 
            |> getResponse
            |> run 
        if (result.statusCode = 200) then
            
            let data = 
                Response.readBodyAsString result
                |> run
        
            Some({
                data = data
                response = result
                fail = false
            })
        else
            None
    
    let createSuccess x =
        {
            Fail = None
            Success = Some(x)
        }
     
    let createFailNoUserIdSupplied x y =
        {
            Fail = Some(NoUserIdSupplied(x))
            Success = None
        }
    let createFailNoFileFound x y =
        {
            Fail = Some(FileNotFound(x))
            Success = None
        }
    let listFiles userId = 
        let result =
            Request.createUrl Get (concatString ["http://localhost:8085/file/list?userId="; userId;])
            |> getResponse
            |> run
        
        match result.statusCode with
            | s when s = 200 -> Response.readBodyAsString result |> run |> Json.deserialize<ApiResponseModels.listFilesResponse> |> createSuccess
            | s when s = 400 -> Response.readBodyAsString result |> run |> createFailNoUserIdSupplied 400
           
                   

    let fileMetaInformationByFileId userId fileId = 
        let result =
            Request.createUrl Get (concatString ["http://localhost:8085/file/meta?userId="; userId;  "&id=";  fileId])
            |> getResponse
            |> run
        match result.statusCode with
            | s when s = 200 -> Response.readBodyAsString result |> run |> Json.deserialize<ApiResponseModels.metadata> |> createSuccess
            | s when s = 404 -> Response.readBodyAsString result |> run |> createFailNoFileFound 404
            | s when s = 401 -> Response.readBodyAsString result |> run |> createFailNoFileFound 404
            | s when s = 400 -> Response.readBodyAsString result |> run |> createFailNoFileFound 404

    let fileMetaInformationByFileName userId parentId fileName = 
        
        let result =
            Request.createUrl Get (concatString ["http://localhost:8085/file/meta?userId="; userId;  "&parent_id=";  parentId; "&name="; fileName]) 
            |> getResponse
            |> run
        
        if (result.statusCode = 200) then
            
            let data = 
                Response.readBodyAsString result
                |> run
        
            Some({
                data = data
                response = result
                fail = false
            })
        else
            None

    let directoryStructure userId = 
        let result =
            Request.createUrl Get (concatString ["http://localhost:8085/dir/structure?userId="; userId;])
            |> getResponse
            |> run
        
        let data =
            Response.readBodyAsString result
            |> run
            |> Json.deserialize<List<ApiResponseModels.directoryStructure>>
        {
            data = Some data
            response = result
            fail = false
        }

    let versionCheck userId clientVersion = 
        
        let result =
            Request.createUrl Get (concatString ["http://localhost:8085/version?userId="; userId;  "&version=";  clientVersion]) 
            |> getResponse
            |> run
        {
            data = None
            response = result
            fail = false
        }
    
    let createFile userId parentId fileName fileTimestamp = 
        let result =
            Request.createUrl Get (concatString ["http://localhost:8085/file?userId="; userId;  "&parent_id=";  parentId; "&name="; fileName; "&timestamp="; fileTimestamp]) 
            |> getResponse
            |> run
        
        let data =
            Response.readBodyAsString result
            |> run
            |> Json.deserialize<ApiResponseModels.createFile>
        {
            data = Some data
            response = result
            fail = false
        }   

    let fileMove userId fileId fileVersion parentId newFilename = 
        let result =
            Request.createUrl Get (concatString ["http://localhost:8085/file/move?userId="; userId;  "&id=";  fileId; "&version="; fileVersion; "&parentId="; parentId; "&name="; newFilename]) 
            |> getResponse
            |> run
        
        let data =
            Response.readBodyAsString result
            |> run
            |> Json.deserialize<ApiResponseModels.moveFile>
        
        {
            data = Some data
            response = result
            fail = false
        }

    let updateFileTimestamp userId fileId fileVersion fileTimestamp = 
        let result =
            Request.createUrl Get (concatString ["http://localhost:8085/file/timestamp?userId="; userId;  "&id=";  fileId; "&version="; fileVersion; "&timestamp="; fileTimestamp]) 
            |> getResponse
            |> run
        
        let data =
            Response.readBodyAsString result
            |> run
            |> Json.deserialize<ApiResponseModels.updateFileTimeStamp>
        
        {
            data = Some data
            response = result
            fail = false
        }
    
    let fileupload content userId id fileId version timestamp =
        let result =
            Request.createUrl Post (concatString  ["http://localhost:8085/file/upload?userId="; userId; "&id="; id; "&fileId="; fileId; "&version="; version;"&timestamp="; timestamp;])
            |> Request.bodyString content
            |> getResponse    
            |> run
        if result.statusCode = 200 then
            let data =
                Response.readBodyAsString result
                |> run
                |> Json.deserialize<ApiResponseModels.fileUpload>
            {
            data = Some data
            response = result
            fail = false
            }
            
        else
            {
            data = None
            response = result
            fail = true
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
            |> Json.deserialize<ApiResponseModels.fileLock>
        
        {
            data = Some data
            response = result
            fail = false
        }

    let directoryCreate userId directoryId directoryName directoryVersion = 
        let result =
            Request.createUrl Get (concatString ["http://localhost:8085/dir?userId="; userId;  "&parentId=";  directoryId; "&name="; directoryName; "&version="; directoryVersion]) 
            |> getResponse
            |> run
        
        let data =
            Response.readBodyAsString result
            |> run
            |> Json.deserialize<ApiResponseModels.directoryCreate>
        
        {
            data = Some data
            response = result
            fail = false
        }

    let directoryMove userId directoryId directoryVersion directoryName parentDirectoryId parentDirectoryVersion = 
        let result =
            Request.createUrl Get (concatString ["http://localhost:8085/dir/move?userId="; userId;  "&id=";  directoryId; "&version="; directoryName; "&name="; directoryVersion; "&parentId="; parentDirectoryId; "&parentVersion="; parentDirectoryVersion]) 
            |> getResponse
            |> run
        
        let data =
            Response.readBodyAsString result
            |> run
            |> Json.deserialize<ApiResponseModels.directoryMove>
        
        {
            data = Some data
            response = result
            fail = false
        }

    let fileDelete userId fileId fileVersion = 
        let result =
            Request.createUrl Get (concatString ["http://localhost:8085/file?userId="; userId;  "&id=";  fileId; "&version="; fileVersion]) 
            |> getResponse
            |> run
        
        let data =
            Response.readBodyAsString result
            |> run
            |> Json.deserialize<ApiResponseModels.fileDelete>

        {
            data = Some data
            response = result
            fail = false
        }
    
    let directoryDelete userId directoryId directoryVersion = 
        let result =
            Request.createUrl Get (concatString ["http://localhost:8085/dir?userId="; userId;  "&id=";  directoryId; "&version="; directoryVersion]) 
            |> getResponse
            |> run
        
        let data =
            Response.readBodyAsString result
            |> run
            |> Json.deserialize<ApiResponseModels.directoryDelete>
        
        {
            data = Some data
            response = result
            fail = false
        }


