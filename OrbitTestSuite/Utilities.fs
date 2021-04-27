namespace OrbitTestSuite.Utilities
open System
open System.Collections.Generic
open System.IO
open OrbitTestSuite.API
open OrbitTestSuite.Models.ApiResponseModels
open OrbitTestSuite.Models.Model
open FsCheck
module Utilities = 
    let uploadFileModel (model:Model) content userId fileId =
        let WriteAccess =
            let user = model.users |> List.find (fun e -> e.userId = userId)
            let file = model.files |> List.find (fun e -> e.metadata.id = fileId)
            let access = user.userFiles.TryFind (string file.metadata.parentId)
            match access with
                | None -> None
                | Some permission ->
                    match permission with
                        | CRUD -> Some(true)
                        | R -> None
        let s = match WriteAccess with
            | Some c ->
                let file = model.files |> List.map (fun e -> if (e.metadata.id = (int fileId)) then {e with content = content;  metadata = {e.metadata with version = e.metadata.version+1; versionChanged = e.metadata.version+1}  } else e)
                let user = model.users |> List.find (fun e -> e.userId = userId)
                let restUsers = model.users |> List.where (fun e -> e.userId <> userId)
                let listfiles = user.listFiles |> List.map (fun e -> if e.id = (int fileId) then {e with version = e.version+1; versionChanged = e.version+1} else e)
                let changedUser = {user with listFiles = listfiles}
                Some({model with files = file; users = changedUser::restUsers})
            | None -> None
        match s with
            | Some c -> {Fail = None; Success = Some(c)}
            | None ->   {Fail = Some(Unauthorized(401)); Success = None} 
        

    let uploadFileSut model content userId fileId =
        let version =
            let user = model.users |> List.find (fun e -> e.userId = userId)
            let file = model.files |> List.find (fun e -> e.metadata.id = fileId)
            let access = user.userFiles.TryFind (string file.metadata.parentId)
            match access with
                | None -> Some(model.files |> List.find (fun e -> e.metadata.id = (int fileId)))    
                | Some permission ->
                    match permission with
                        | CRUD -> Some(model.files |> List.find (fun e -> e.metadata.id = (int fileId)))
                        | R -> None
        match version with
            | Some c -> API.fileUpload content userId (string fileId) (string (int c.metadata.version-1)) "637479675580000000"
            
            
    let downloadFileModel model userId fileId =
        let version =
            let user = model.users |> List.find (fun e -> e.userId = userId)
            let file = model.files |> List.find (fun e -> e.metadata.id = fileId)
            let access = user.userFiles.TryFind (string file.metadata.parentId)
            match access with
                | None -> None
                | Some permission ->
                    match permission with
                        | CRUD -> Some(model.files |> List.find (fun e -> e.metadata.id = (int fileId)))
                        | R -> Some(model.files |> List.find (fun e -> e.metadata.id = (int fileId)))
        match version with
            | None -> {Fail = Some(Unauthorized(401)); Success = None}
            | Some c -> {Fail = None; Success = Some(c.content)}
       
    let listFilesModel (model:Model) userId =
       let user = model.users |> List.find (fun e -> e.userId = userId)
       {Fail = None; Success = Some({directoryVersions = user.directoryVersions; fileList = user.listFiles})}

    let fileMetaInformationModel (model:Model) userId fileId =
        let readAccess =
            let user = model.users |> List.find (fun e -> e.userId = userId)
            let file = model.files |> List.find (fun e -> e.metadata.id = fileId)
            let access = user.userFiles.TryFind (string file.metadata.parentId)
            match access with
                | None -> false
                | Some permission ->
                    match permission with
                        | CRUD -> true
                        | R -> true
        if readAccess
            then
                let file = model.users |> List.find (fun e -> e.userId = userId) 
                let metaData = model.files |> List.tryFind (fun e -> e.metadata.id = (int fileId))
                match metaData with
                    | Some meta -> {Fail = None; Success = Some(meta.metadata)}
                    | None -> {Fail = Some(FileNotFound(404)); Success = None}
            else
                {Fail = Some(Unauthorized(401)); Success = None}
    let dirStructureModel (model:Model) userId =
        let user = model.users |> List.find (fun e -> e.userId = userId)
        {Fail = None; Success = Some(user.dirStructures)}
    let fileMetaInformationSut userId fileId =
        let fileMetaInformation = (API.fileMetaInformationByFileId userId fileId)
        fileMetaInformation
    
    let createFileModel (model:Model) userId (dirId:int) fileName =
        let readAccess =
            let user = model.users |> List.find (fun e -> e.userId = userId)
            let access = user.userFiles.TryFind (string dirId)
            match access with
                | None -> false
                | Some permission ->
                    match permission with
                        | CRUD -> true
                        | R -> false
        let fileExists =
            let exists = model.files |> List.tryFind (fun e -> e.metadata.name = fileName && e.metadata.parentId = dirId)
            match exists with
               | Some exi -> true
               | None -> false 
        
        let file = {content = ""; metadata = {id = model.currentFileId; parentId =  dirId; version = 1; versionChanged = 1; timestamp = "637479675580000000"; name=fileName}}::[]
        if readAccess && not fileExists
        then
            let user = model.users |> List.find (fun e -> e.userId = userId)
            let notUser = model.users |> List.find (fun e-> e.userId <> userId)
            let restUsers = notUser::[]
            let modifiedUser = {user with listFiles = user.listFiles@{id = model.currentFileId; parentId = dirId; version = 1;versionChanged = 1; timestamp = "637479675580000000"; name=fileName}::[]}
            {Fail = None; Success = Some({model with files = model.files@file;currentFileId = model.currentFileId+1; users = restUsers@modifiedUser::[]})}
        elif fileExists
        then
             {Fail = Some(FileAlreadyExist(409)); Success = Some({model with currentFileId = model.currentFileId+1})}
        elif readAccess then   
            {Fail = Some(Unauthorized(401)); Success = Some({model with currentFileId = model.currentFileId+1})}
        else
            {Fail = Some(Unauthorized(401)); Success = Some({model with currentFileId = model.currentFileId+1})}
    
    let moveFileModel (model:Model) userId fileId (dirId:int) fileName =
        let WriteAccess =
            let user = model.users |> List.find (fun e -> e.userId = userId)
            let file = model.files |> List.find (fun e -> e.metadata.id = fileId)
            let fileAccess = user.userFiles.TryFind (string file.metadata.parentId)
            let dirAccess = user.userFiles.TryFind (string dirId)
            match fileAccess , dirAccess with
                | None , None -> None
                | Some file , Some dir ->
                    match file , dir with
                        | CRUD , CRUD -> Some(true)
                        | _ , _ -> None
                | _ , _ -> None
        let s = match WriteAccess with
            | Some c ->
                let file = model.files |> List.map (fun e -> if (e.metadata.id = (int fileId)) then {e with metadata = {e.metadata with version = e.metadata.version+1; parentId = dirId; name = fileName}  } else e)
                let user = model.users |> List.find (fun e -> e.userId = userId)
                let restUsers = model.users |> List.where (fun e -> e.userId <> userId)
                let listFiles = user.listFiles |> List.map (fun e -> if e.id = (int fileId) || e.parentId = (int dirId)  then {e with version = e.version+1; parentId = dirId; name = fileName} else e)
                let changedUser = {user with listFiles = listFiles}
                Some({model with files = file; users = changedUser::restUsers})
            | None -> None
        match s with
            | Some c -> {Fail = None; Success = Some(c)}
            | None ->   {Fail = Some(Unauthorized(401)); Success = None}
            
    let MoveFileSut (model:Model) userId fileId (dirId:int) fileName =
        let version =
            let user = model.users |> List.find (fun e -> e.userId = userId)
            let file = model.files |> List.find (fun e -> e.metadata.id = fileId)
            let access = user.userFiles.TryFind (string file.metadata.parentId)
            match access with
                | None -> Some(model.files |> List.find (fun e -> e.metadata.id = (int fileId)))    
                | Some permission ->
                    match permission with
                        | CRUD -> Some(model.files |> List.find (fun e -> e.metadata.id = (int fileId)))
                        | R -> None
        match version with
            | Some c ->
                let s = API.fileMove userId (string fileId) (string (int c.metadata.version-1)) (string dirId) fileName
                s
    let compareMetadata (x:Modelmetadata) (y:metadata) =
        (x.id = (string y.id) && x.name = y.name && x.version = y.version && x.parentId = (string y.parentId) && x.timeStamp = y.timestamp && x.versionChanged = y.versionChanged)
        
    
    let rec compareFileList (xl:Modelmetadata list) (yl:metadata list) = 
         match xl, yl with 
            | [] , [] -> true
            | x::xs, y::ys -> compareMetadata x y && compareFileList xs ys
            | _ -> false
    
    let getCurrentFileId (l:File list) =
        let mutable currentFileId = 0
        currentFileId <- l.Head.metadata.id
        l |> List.map (fun e -> if (e.metadata.id > currentFileId) then currentFileId <- e.metadata.id else ())
        currentFileId
        
    let getFileListAPI =
        let fileList1 = API.listFiles "100"
        let file1Res = fileList1.Success.Value.fileList |> List.map (fun e -> {content = ""; metadata = {id = e.id; name = e.name; timestamp = e.timestamp; version = e.version; versionChanged = e.versionChanged; parentId = e.parentId}})
        let fileList2 = API.listFiles "101"
        let file2Res = fileList2.Success.Value.fileList |> List.map (fun e -> {content = ""; metadata = {id = e.id; name = e.name; timestamp = e.timestamp; version = e.version; versionChanged = e.versionChanged; parentId = e.parentId}})
        let fileList3 = API.fileMetaInformationByFileName "100" "9" "intro.txt"
        let file3Res = {content = ""; metadata = {id = fileList3.Success.Value.id; name = fileList3.Success.Value.name; timestamp = fileList3.Success.Value.timestamp; version = fileList3.Success.Value.version; versionChanged = fileList3.Success.Value.versionChanged; parentId = fileList3.Success.Value.parentId}}::[]
        file3Res@file2Res@file1Res
    let getTestData =
                let listFilesResult = API.listFiles "100"
                let temp =
                    Some(listFilesResult.Success.Value.fileList).Value |> List.map (fun e ->
                        let s = API.downloadFile "100" (string e.id)
                        {
                        userFiles = Map.empty
                        listFiles = [{id =  e.id; name = e.name; parentId =  e.parentId; version = e.version; versionChanged = e.versionChanged; timestamp = e.timestamp}]
                        directoryVersions = []
                        dirStructures = []
                        userId = "100"
                    }
                    )
                let dir = Some(listFilesResult.Success.Value.directoryVersions).Value |> List.map (fun e -> {
                    id =  e.id
                    version =  e.version
                } )
                let hhh = API.directoryStructure "100"
                let dirStruc0 = match hhh.Success with
                    | Some c -> c 
                let s = temp |> List.map (fun  e -> {e with directoryVersions = dir}) |> List.map (fun e -> { e with dirStructures = dirStruc0 })
                let g = s |> List.map (fun e -> {e with userFiles = e.userFiles.Add("15" , CRUD).Add("9" , CRUD).Add("18", CRUD).Add("17",CRUD)})
                let listFilesResult = API.listFiles "101"
                let temp =
                    Some(listFilesResult.Success.Value.fileList).Value |> List.map (fun e ->
                    let s = API.downloadFile "101" (string e.id)
                    {
                    userFiles = Map.empty
                    listFiles = [{id =  e.id; name = e.name; parentId =  e.parentId; version = e.version; versionChanged = e.versionChanged; timestamp = e.timestamp}]
                    directoryVersions = []
                    dirStructures = []
                    userId = "101"
                    }
                    )
                let dir = Some(listFilesResult.Success.Value.directoryVersions).Value |> List.map (fun e -> {
                    id =  e.id
                    version =  e.version
                } )
                let sss = API.directoryStructure "101"
                let dirStruc1 = match sss.Success with
                    | Some c -> c 
                let v = temp |> List.map (fun  e -> {e with directoryVersions = dir}) |> List.map (fun e -> { e with dirStructures = dirStruc1 })
                let l = v |> List.map (fun e -> {e with userFiles = e.userFiles.Add("16" , CRUD).Add("9" , R).Add("18", R).Add("17",R)})
                let listFilesResult1 = API.listFiles "100"
                let listFilesResult2 = API.listFiles "101"
                let temp1 =
                    Some(listFilesResult1.Success.Value.fileList).Value |> List.map (fun e ->
                    let s = API.downloadFile "100" (string e.id)
                    {metadata = {id =  e.id; name = e.name; parentId =  e.parentId; version = e.version; versionChanged = e.versionChanged; timestamp = e.timestamp}; content = match s.Success with
                        | Some c -> c
                        }
                        )
                let temp2 =
                    Some(listFilesResult2.Success.Value.fileList).Value |> List.map (fun e ->
                    let s = API.downloadFile "101" (string e.id)
                    {metadata = {id =  e.id; name = e.name; parentId =  e.parentId; version = e.version; versionChanged = e.versionChanged; timestamp = e.timestamp}; content = match s.Success with
                        | Some c -> c
                        }
                        )
                let addedFile4 = {metadata = {id = 4; version = 1; versionChanged = 1; name = "INTRO.txt";parentId = 9; timestamp = "637479675580000000"}; content = "INTRO.txt located at /Users/Shared files/INTRO.txt
USER_ID=100 (rw) can read and write content to the file, but USER_ID=101 (ro) can only read it. USER_ID=102 (none) has no access it."}::[]
                {users = g@l; files = temp1@temp2; currentFileId = getCurrentFileId (temp1@temp2)}