namespace OrbitTestSuite.Utilities
open System
open System.Collections.Generic
open System.IO
open OrbitTestSuite.API
open OrbitTestSuite.Models.ApiResponseModels
open OrbitTestSuite.Models.Model
open FsCheck
module Utilities =
    
    let rec getUser (list:User list) userId = match list with
        | user::users -> if user.userId = userId then Some(user) else getUser users userId
        | [] -> None
    
    
    
    let uploadFileModel (model:Model) content userId fileId =
        let WriteAccess =
            let user = getUser model.users userId
            let file = model.files |> List.tryFind (fun e -> e.metadata.id = fileId)
            let access = match file, user with
                | None, None -> None
                | Some fi, Some user -> user.userFiles.TryFind (string fi.metadata.parentId)
                | None , Some user -> None
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
                let listfiles = user.listFiles |> List.map (fun e -> if e.id = (int fileId) then {e with version = e.version+1; versionChanged = e.version+1;};   else e)
                let changedUser = {user with listFiles = listfiles}
                let currentFileid = model.files |> List.tryFind (fun e -> e.metadata.id = fileId)
                match currentFileid with
                    | Some id -> Some({model with files = file; users = changedUser::restUsers; currentUpdatedFile = id.metadata.version })
                    | None -> Some({model with files = file; users = changedUser::restUsers; })
                
            | None -> None
        match s with
            | Some c -> {Fail = None; Success = Some(c)}
            | None ->   {Fail = Some(Unauthorized(401)); Success = None} 
        

    let uploadFileSut (model:Model) content userId fileId =
        API.fileUpload content userId (string fileId) (string model.currentUpdatedFile) "637479675580000000"
            
            
    let downloadFileModel model userId fileId =
            let user = model.users |> List.find (fun e -> e.userId = userId)
            let file = model.files |> List.tryFind (fun e -> e.metadata.id = fileId)
            match file with
                | None -> {Fail = Some(FileNotFound(404)); Success = None}
                | Some file ->
                    let permission = user.userFiles.TryFind (string file.metadata.parentId)
                    match permission with
                        | Some CRUD ->
                            let s = model.files |> List.tryFind (fun e -> e.metadata.id = (int fileId))
                            match s with
                             | None -> {Fail = Some(FileNotFound(404)); Success = None}
                             | Some fi -> {Fail = None; Success = Some(fi.content)}
                        | Some R ->
                            let s = model.files |> List.tryFind (fun e -> e.metadata.id = (int fileId))
                            match s with
                             | None -> {Fail = Some(Unauthorized(401)); Success = None}
                             | Some fi -> {Fail = None; Success = Some(fi.content)}
                        | None ->  {Fail = Some(Unauthorized(401)); Success = None} 
    let listFilesModel (model:Model) userId =
       let user = model.users |> List.find (fun e -> e.userId = userId)
       {Fail = None; Success = Some({directoryVersions = user.directoryVersions; fileList = user.listFiles})}

    let fileMetaInformationModel (model:Model) userId fileId =
            let user = model.users |> List.find (fun e -> e.userId = userId)
            let file = model.files |> List.tryFind (fun e -> e.metadata.id = fileId)
            match file with
                | None -> {Fail = Some(FileNotFound(404)); Success = None}
                | Some file ->
                    let permission = user.userFiles.TryFind (string file.metadata.parentId)
                    match permission with
                        | Some CRUD ->
                            let file = model.users |> List.find (fun e -> e.userId = userId) 
                            let metaData = model.files |> List.tryFind (fun e -> e.metadata.id = (int fileId))
                            match metaData with
                                | Some meta -> {Fail = None; Success = Some(meta.metadata)}
                                | None -> {Fail = Some(FileNotFound(404)); Success = None}
                        | Some R ->
                            let file = model.users |> List.find (fun e -> e.userId = userId) 
                            let metaData = model.files |> List.tryFind (fun e -> e.metadata.id = (int fileId))
                            match metaData with
                                | Some meta -> {Fail = None; Success = Some(meta.metadata)}
                                | None -> {Fail = Some(FileNotFound(404)); Success = None}
                        | None ->{Fail = Some(Unauthorized(401)); Success = None}
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
        let user = model.users |> List.find (fun e-> e.userId = userId)
        let file = model.files |> List.tryFind (fun e -> e.metadata.id = fileId)
        let userWriteAccess = match file with
           | None -> None
           | Some fi -> user.userFiles.TryFind (string fi.metadata.parentId)
        let userMoveAccess = user.userFiles.TryFind (string dirId)
        let fileAlreadyExists = model.files |> List.tryFind (fun e -> e.metadata.name = fileName && e.metadata.parentId =  dirId)
                
        match userWriteAccess , userMoveAccess , fileAlreadyExists with
            | Some wa , Some ma , None ->
                let file = model.files |> List.map (fun e -> if (e.metadata.id = (int fileId)) then {e with metadata = {e.metadata with version = e.metadata.version+1; parentId = dirId; name = fileName}  } else e)
                let user = model.users |> List.find (fun e -> e.userId = userId)
                let restUsers = model.users |> List.where (fun e -> e.userId <> userId)
                let listfiles = user.listFiles |> List.map (fun e -> if e.id = (int fileId) then {e with version = e.version+1; parentId = dirId; name = fileName } else e)
                let changedUser = {user with listFiles = listfiles}
                let s = Some({model with files = file; users = changedUser::restUsers})
                {Fail = None; Success = s}
            | _ , _ , _ -> {Fail = Some(Unauthorized(401)); Success = None}
        
    let MoveFileSut (model:Model) userId fileId (dirId:int) fileName =
        let user = model.users |> List.find (fun e -> e.userId = userId)
        let file = model.files |> List.tryFind (fun e -> e.metadata.id = fileId)
        match file with
           | None -> {Fail = Some(Unauthorized(401)); Success = None}
           | Some fi -> API.fileMove userId (string fileId) (string (int fi.metadata.version-1)) (string dirId) fileName
        

    let updateFileTimestampModel (model:Model) userId fileId timeStamp  =
        let user = model.users |> List.find (fun e -> e.userId = userId)
        let file = model.files |> List.tryFind (fun e -> e.metadata.id = fileId)
        let userWriteAccess = match file with
           | None -> None
           | Some fi -> user.userFiles.TryFind (string fi.metadata.parentId)
        
        match userWriteAccess with
            | Some access ->
                let file = model.files |> List.map (fun e -> if (e.metadata.id = (int fileId)) then {e with metadata = {e.metadata with version = e.metadata.version+1; timestamp = timeStamp}  } else e)
                let user = model.users |> List.find (fun e -> e.userId = userId)
                let restUsers = model.users |> List.where (fun e -> e.userId <> userId)
                let listfiles = user.listFiles |> List.map (fun e -> if e.id = (int fileId) then {e with version = e.version+1; timestamp = timeStamp } else e)
                let changedUser = {user with listFiles = listfiles}
                let s = Some({model with files = file; users = changedUser::restUsers})
                {Fail = None; Success = s}
            | None ->  {Fail = Some(Unauthorized(401)); Success = None}

    let updateFileTimestampSut (model:Model) userId fileId timeStamp =
        let user = model.users |> List.find (fun e -> e.userId = userId)
        let file = model.files |> List.tryFind (fun e -> e.metadata.id = fileId)
        match file with
           | None -> {Fail = Some(Unauthorized(401)); Success = None}
           | Some fi -> API.updateFileTimestamp userId (string fileId) (string (int fi.metadata.version-1)) timeStamp
        
    let fileDeleteModel (model:Model) userId fileId  =
        let user = model.users |> List.find (fun e -> e.userId = userId)
        let file = model.files |> List.tryFind (fun e -> e.metadata.id = fileId)
        let userWriteAccess = match file with
            | None -> None
            | Some fi -> user.userFiles.TryFind (string fi.metadata.parentId)
        
        match userWriteAccess with
            | Some CRUD ->
                let fileToDelete = model.files |> List.find (fun e -> e.metadata.id = fileId )
                let file = model.files |> List.filter (fun e -> e.metadata.id <> fileId )
                let user = model.users |> List.find (fun e -> e.userId = userId)
                let restUsers = model.users |> List.where (fun e -> e.userId <> userId)
                let listfiles = user.listFiles |> List.filter (fun e -> e.id <> fileId)
                let changedUser = {user with listFiles = listfiles}
                let s = Some({model with files = file; users = changedUser::restUsers; deletedFileVersion = fileToDelete.metadata.version })
                {Fail = None; Success = s}
            | Some R ->  {Fail = Some(Unauthorized(401)); Success = None}
            | None ->  {Fail = Some(Unauthorized(401)); Success = None}
            
    let deleteFileSut (model:Model) userId fileId =
        let s = API.fileDelete userId (string fileId) (string model.deletedFileVersion)
        s
        
        
    let createDirectoryModel (model:Model) userId (dirId:int) dirName = 
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
            let user = model.users |> List.find (fun e -> e.userId = userId)
            let exists = user.dirStructures |> List.tryFind (fun e -> e.name = dirName )
            match exists with
               | Some exi -> true
               | None -> false 
        
        let file = {rootId = dirId; id = model.currentDirId; name = dirName; rootPath = ""; rootIsDefault = false; version = 1; parentId = Some(dirId)}::[]
        if readAccess && not fileExists
        then
            let user = model.users |> List.find (fun e -> e.userId = userId)
            let notUser = model.users |> List.find (fun e-> e.userId <> userId)
            let restUsers = notUser::[]
            let modifiedUser = {user with dirStructures = user.dirStructures@file; userFiles = user.userFiles.Add((string model.currentDirId), CRUD)}::[]
            let s = Some({dir = (string model.currentDirId); rights = Some(CRUD); user = userId})::[]
            let c = model.rights
            {Fail = None; Success = Some({model with users = modifiedUser@restUsers; currentDirId = model.currentDirId+1; rights = c@s })}
        elif fileExists
        then
             {Fail = Some(FileAlreadyExist(409)); Success = Some({model with currentFileId = model.currentDirId+1})}
        elif readAccess then   
            {Fail = Some(Unauthorized(401)); Success = Some({model with currentFileId = model.currentDirId+1})}
        else
            {Fail = Some(Unauthorized(401)); Success = Some({model with currentFileId = model.currentDirId+1})}
    
    let moveDirectoryModel (model:Model) userId (dirId:int) dirName parentDirId =
        let user = model.users |> List.find (fun e-> e.userId = userId)
        let file = user.dirStructures |> List.tryFind (fun e -> e.id = dirId)
        let userWriteAccess = match file with
           | None -> None
           | Some fi -> user.userFiles.TryFind (string parentDirId)
        let userMoveAccess = user.userFiles.TryFind (string dirId)
        let fileAlreadyExists = user.dirStructures |> List.tryFind (fun e -> e.name = dirName && e.id =  dirId)
                
        match userWriteAccess , userMoveAccess , fileAlreadyExists with
            | Some wa , Some ma , None ->
                let file = user.dirStructures |> List.map (fun e -> if (e.id = (int dirId)) then {e with version = e.version; parentId = e.parentId; name = e.name  } else e)
                let user = model.users |> List.find (fun e -> e.userId = userId)
                let restUsers = model.users |> List.where (fun e -> e.userId <> userId)
                let changedUser = {user with dirStructures = file}
                let currentUpdatedDir = file |> List.find (fun e -> e.id = dirId )
                let s = Some({model with  users = changedUser::restUsers; currentUpdatedDirId = currentUpdatedDir.id})
                {Fail = None; Success = s}
            | _ , _ , _ -> {Fail = Some(Unauthorized(401)); Success = None}
            
    let moveDirecotrySut (model:Model) userId (dirId:int) dirName parentDirId  =
        let user = model.users |> List.find (fun e -> e.userId = userId)
        let file = user.dirStructures |> List.tryFind (fun e -> e.id = dirId)
        match file with
           | None -> {Fail = Some(Unauthorized(401)); Success = None}
           | Some fi -> API.directoryMove userId (string dirId) (string (int fi.version-1)) dirName parentDirId (string fi.parentId)
           
(*
    let directoryDeleteModel (model:Model) userId dirId  =
        let user = model.users |> List.find (fun e -> e.userId = userId)
        let file = model.files |> List.tryFind (fun e -> e.metadata.id = fileId)
        let userWriteAccess = match file with
            | None -> None
            | Some fi -> user.userFiles.TryFind (string fi.metadata.parentId)
        
        match userWriteAccess with
            | Some access ->
                let fileToDelete = model.files |> List.find (fun e -> e.metadata.id = fileId )
                let file = model.files |> List.filter (fun e -> e.metadata.id <> fileId )
                let user = model.users |> List.find (fun e -> e.userId = userId)
                let restUsers = model.users |> List.where (fun e -> e.userId <> userId)
                let listfiles = user.listFiles |> List.filter (fun e -> e.id <> fileId)
                let changedUser = {user with listFiles = listfiles}
                let s = Some({model with files = file; users = changedUser::restUsers; deletedFileVersion = fileToDelete.metadata.version })
                {Fail = None; Success = s}
            | None ->  {Fail = Some(Unauthorized(401)); Success = None}
*)
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
    
   
    let getAllDirId  userId =
        let dirStrcutures = API.directoryStructure userId
        let dir = match dirStrcutures.Success , dirStrcutures.Fail with
            | Some dir , None -> dir |> List.map (fun e -> {user = userId; directories = e.id})

        let x = dir |> List.map (fun user ->
                let apires = API.SERVICEDirMetaData userId (string user.directories)
                {user = userId; metadata = apires.Success.Value}

        )

        let res = x  |> List.filter (fun e -> e.user = userId)
        let xxx = res |> List.map (fun e ->
            let s = match e.metadata.__permissions with
                | None -> None
                | Some x -> if (x.create && x.read) then Some {dir = string e.metadata.id; rights = Some CRUD; user = e.user } elif (not x.create && x.read) then Some {dir = string e.metadata.id; rights = Some R; user = e.user } else Some {dir = string e.metadata.id; rights = None; user = e.user }
            s
             )
        xxx
        
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
                           {
                            userFiles = Map.empty
                            listFiles = []
                            directoryVersions = []
                            dirStructures = []
                            userId = "100"
                        }
                    let xxx =
                            Some(listFilesResult.Success.Value.fileList).Value |> List.map (fun e ->
                            let s = API.downloadFile "100" (string e.id)
                            let f =  {id =  e.id; name = e.name; parentId =  e.parentId; version = e.version; versionChanged = e.versionChanged; timestamp = e.timestamp}   
                            f
                            )
                    let temp = {temp with listFiles = xxx}::[]
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
                    let tempx =
                        {
                        userFiles = Map.empty
                        listFiles = []
                        directoryVersions = []
                        dirStructures = []
                        userId = "101"
 
                        }
                    let gg =
                        Some(listFilesResult.Success.Value.fileList).Value |> List.map (fun e ->
                        let s = API.downloadFile "101" (string e.id)
                        let f =  
                         {id =  e.id; name = e.name; parentId =  e.parentId; version = e.version; versionChanged = e.versionChanged; timestamp = e.timestamp}   
                        f
                        )
                    let tempx = {tempx with listFiles = gg}::[]
                    let dir = Some(listFilesResult.Success.Value.directoryVersions).Value |> List.map (fun e -> {
                        id =  e.id
                        version =  e.version
                    } )
                    let sss = API.directoryStructure "101"
                    let dirStruc1 = match sss.Success with
                        | Some c -> c 
                    let v = tempx |> List.map (fun  e -> {e with directoryVersions = dir}) |> List.map (fun e -> { e with dirStructures = dirStruc1 })
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
                    let file4Meta = API.fileMetaInformationByFileId "100" "4"
                    let file4Content = API.downloadFile "100" "4"
                    let files = match file4Meta.Success , file4Meta.Fail , file4Content.Success , file4Content.Fail with
                        | Some meta , None , Some cont , None  -> temp1@temp2@{metadata = {id = meta.id; version = meta.version; versionChanged = meta.versionChanged; parentId = meta.parentId; timestamp = meta.timestamp; name = meta.name}; content = cont}::[]
                        | None , Some s , None , Some e  -> temp1@temp2
                        | _ , _ , _ , _f  -> temp1@temp2
                    //let currentfileId = (Utilities.getCurrentFileId Utilities.getFileListAPI)+1
                    {users = g@l; files = files; currentFileId = 0; deletedFileVersion = 0; currentUpdatedFile = 0; currentDirId = 0; currentUpdatedDirId = 0; rights = []}