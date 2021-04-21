namespace OrbitTestSuite.Utilities
open System.Collections.Generic
open OrbitTestSuite.API
open OrbitTestSuite.Models.ApiResponseModels
open OrbitTestSuite.Models.ApiResponseModels
open OrbitTestSuite.Models.ApiResponseModels
open OrbitTestSuite.Models.ApiResponseModels
open OrbitTestSuite.Models.Model
open FsCheck
module Utilities = 
    let uploadFileModel model content userId fileId =
         try 
            let s = model.users |> List.find (fun user -> user.userId = userId)
            let a = model.users |> List.where (fun user -> user.userId <> userId)
            let qw = s.files  |> List.find (fun file -> file.fileId = fileId)
            let sw = s.fileList |> List.map (fun e -> if (e.id = (int fileId)) then {e with version = e.version+1; versionChanged = e.versionChanged+1} else e)
            let b = {qw with content = content; fileVersion = qw.fileVersion+1;} 
            let x = {s with files = b::[]; fileList = sw }
            let v = a@x::[]
            Some{model with users = v}
         with
         | :? KeyNotFoundException -> None
     
    let uploadFileModelWithBug model content userId fileId =
      try 
            let s = model.users |> List.find (fun user -> user.userId = userId)
            let a = model.users |> List.where (fun user -> user.userId <> userId)
            let qw = s.files  |> List.find (fun file -> file.fileId = fileId)
            let b = if(qw.fileVersion<50) then {qw with content = content; fileVersion = qw.fileVersion+1} else {qw with content = ""; fileVersion = qw.fileVersion+1} // Silly bug
            let x = {s with files = b::[] }
            let v = a@x::[]
            {fail = false; content = Some{model with users = v}}
         with
         | :? KeyNotFoundException -> {fail = true; content = None}
        
    let uploadFileSut model content userId fileId =
        try 
            let user = model.users |> List.find (fun e-> e.userId = (string userId))
            let file = user.files |> List.find (fun file -> file.fileId =  fileId)
            let currentFileversion = file.fileVersion-1
            let currentFileid = file.fileId
            Some(API.fileupload content userId  currentFileid  (string currentFileversion) (string currentFileversion) "637479675580000000")
        with
         | :? KeyNotFoundException -> None
        
    let downloadFileModel model userId fileId =
         let localRes = model.users |> List.find (fun e -> e.userId = userId)
         let s = localRes.files |> List.tryFind (fun e-> e.fileId =  fileId)
         s
    let downloadFileSut userId fileId =
        let Apires = (API.downloadFile userId fileId)
        Apires
        
    let listFilesModel model userId =
        let rec findFileList users userId = match users with
            | [] -> {Fail = None; Success = Some({directoryVersions = []; fileList = []})}
            | el::els -> if el.userId = userId then {Fail = None; Success = Some({directoryVersions = el.directoryVersions; fileList = el.fileList})} else findFileList els userId
        let fileList = findFileList model.users userId
        fileList
    let listFilesSut userId =
        API.listFiles userId
        
    let fileMetaInformationModel (model:Model) userId fileId =
        try 
            let user = model.users |> List.find (fun e -> e.userId = userId)
            let file = user.fileList |> List.find (fun e -> e.id = fileId)
            {Fail = None; Success = Some{ id = file.id; version = file.version; versionChanged =  file.versionChanged;name =  file.name;parentId = file.parentId; timestamp = file.timestamp}}
        with
         | :? KeyNotFoundException -> {Fail = Some(FileNotFound(404)); Success = None}

    
    let fileMetaInformationSut userId fileId =
        let fileMetaInformation = (API.fileMetaInformationByFileId userId fileId)
        fileMetaInformation
        
    let compareMetadata (x:Modelmetadata) (y:metadata) =
        (x.id = (string y.id) && x.name = y.name && x.version = y.version && x.parentId = (string y.parentId) && x.timeStamp = y.timestamp && x.versionChanged = y.versionChanged)
        
    
    let rec compareFileList (xl:Modelmetadata list) (yl:metadata list) = 
         match xl, yl with 
            | [] , [] -> true
            | x::xs, y::ys -> compareMetadata x y && compareFileList xs ys
            | _ -> false
    
    
    let getTestData =
         let listFilesResult = API.listFiles "100"
         let temp =
             Some(listFilesResult.Success.Value.fileList).Value |> List.map (fun e ->
                let s = API.downloadFile "100" (string e.id)
                {
                files = [{fileVersion = e.version; content = match s with
                       | Some c -> c.data
                       ; fileId = (string e.id)};]
                fileList = [{id =  e.id; name = e.name; parentId =  e.parentId; version = e.version; versionChanged = e.versionChanged; timestamp = e.timestamp}]
                directoryVersions = []
                userId = "100"
                }
                )
         let dir = Some(listFilesResult.Success.Value.directoryVersions).Value |> List.map (fun e -> {
             id =  e.id
             version =  e.version
            }      )
         let s = temp |> List.map (fun  e -> {e with directoryVersions = dir})
         let listFilesResult = API.listFiles "101"
         let temp =
             Some(listFilesResult.Success.Value.fileList).Value |> List.map (fun e ->
                let s = API.downloadFile "101" (string e.id)
                {
                files = [{fileVersion = e.version; content = match s with
                       | Some c -> c.data
                       ; fileId = (string e.id)};]
                fileList = [{id =  e.id; name = e.name; parentId =  e.parentId; version = e.version; versionChanged = e.versionChanged; timestamp = e.timestamp}]
                directoryVersions = []
                userId = "101"
                }
                )
         let dir = Some(listFilesResult.Success.Value.directoryVersions).Value |> List.map (fun e -> {
             id =  e.id
             version =  e.version
         } )
         let v = temp |> List.map (fun  e -> {e with directoryVersions = dir})
         {users = s@v}
                     
          