namespace OrbitTestSuite.Utilities
open OrbitTestSuite.API
open OrbitTestSuite.Models.ApiResponseModels
open OrbitTestSuite.Models.Model

module Utilities =
    
    let rec tryGetUser (list:User list) userId = match list with
        | user::users -> if user.userId = userId then Some(user) else tryGetUser users userId
        | [] -> None
   
    let rec tryGetFileInSameDirWithSameName (list:File list) fileName dirId = match list with
        | file::files -> if (file.metadata.parentId <> dirId && file.metadata.name = fileName) then Some(file) else tryGetFileInSameDirWithSameName files fileName dirId
        | [] -> None
    let rec tryGetCheckedOutDir (list:directoryVersion list) dirId = match list with
        | dir::dirs -> if dir.id = dirId then Some(dir) else tryGetCheckedOutDir dirs dirId
        | [] -> None
    let rec tryGetFileById (list:File list) fileId = match list with
        | file::files -> if fileId  = file.metadata.id then Some(file) else tryGetFileById files fileId
        | [] -> None
    let rec tryGetFileByName (list:File list) fileName = match list with
        | file::files -> if fileName  = file.metadata.name then Some(file) else tryGetFileByName files fileName
        | [] -> None
    let rec tryGetFileByNameAndDir (list:File list) fileName dirId = match list with
        | file::files -> if fileName  = file.metadata.name && dirId = file.metadata.parentId then Some(file) else tryGetFileByNameAndDir files fileName dirId
        | [] -> None
    let rec tryGetRights (list:dirAndRights list) dirId userId = match list with
        | dirAndRight::dirAndRights -> if (dirAndRight.dir = dirId && userId = dirAndRight.user) then Some(dirAndRight.rights) else tryGetRights dirAndRights dirId userId
        | [] -> None
    let rec tryGetParent (list:directoryStructure list) id = match list with
        | dir::dirs -> if (dir.id = id) then Some(dir) else tryGetParent dirs id
        | [] -> None
    
    let rec tryGetDirectory (list:directoryStructure list) dirId = match list with
        | dir::dirs -> if (dir.id = dirId) then Some(dir) else tryGetDirectory dirs dirId
        | [] -> None
        
    
    let rec getAllFileIds (files:File list) =
        match files with
            | [] -> []
            | file::files -> file.metadata.id::getAllFileIds files
    let rec getAllFileVersions (files:File list) =
        match files with
            | [] -> []
            | file::files -> file.metadata.version::getAllFileVersions files
    let rec getALlDirIds (list:directoryStructure list) =  match list with
            | [] -> []
            | dir::dirs -> dir.id::getALlDirIds dirs
    

    let uploadFileModel (model:Model) content userId fileId fileversion timeStamp =
        let file = tryGetFileById model.files fileId
        match file with
            | None -> {Fail = Some(NotFound); Success = None}
            | Some file ->
                if file.metadata.version <> fileversion
                then {Fail= Some(Conflict); Success = None}
                else 
                let rights = tryGetRights model.rights (string file.metadata.parentId) userId
                match rights with
                | None -> {Fail= Some(Unauthorized); Success = None}
                | Some permission -> match permission with
                    | NonePermission -> {Fail= Some(Unauthorized); Success = None}
                    | R -> {Fail= Some(Unauthorized); Success = None}
                    | CRUD ->
                        let users = model.users |> List.map (fun e ->
                                  if (e.userId = userId)
                                  then {e with listFiles = e.listFiles |> List.map (fun s -> if (s.id = fileId) then {s with version = s.version+1; versionChanged = s.version+1} else s) }
                                  else e )
                        let files = model.files  |> List.map (fun e ->
                                  if (e.metadata.id = fileId)
                                  then {e with content = content; metadata = {e.metadata with version = e.metadata.version+1;versionChanged=e.metadata.version+1 }}
                                  else e )
                        {Fail = None; Success = Some({model with users = users; files = files; sutResponse = Some(UploadFileSuccess{id=file.metadata.id; version = file.metadata.version+1; timestamp = timeStamp })})}
    
    let uploadFileSut (model:Model) content userId fileId =
        API.fileUpload content userId (string fileId) (string model.currentUpdatedFile) "637479675580000000"
    
    let downloadFileModel model userId fileId =
            let file = tryGetFileById model.files fileId
            match file with
                    | None -> {Fail = Some(NotFound); Success = None}
                    | Some file ->
                        let dirAndRights = model.rights |> List.tryFind (fun e -> e.dir = (string file.metadata.parentId) && e.user = userId)
                        match dirAndRights with
                           | None ->  {Fail = Some(Unauthorized); Success = None}
                           | Some permission -> match permission.rights with
                                | CRUD -> {Fail = None; Success = Some(file.content)}
                                | R -> {Fail = None; Success = Some(file.content)}
                                | NonePermission ->  {Fail = Some(Unauthorized); Success = None} 
    
    (*
    let moveDirectoryModel (model:Model) userId (dirId:int) dirName parentDirId dirVersion parentVersion =
        let dirToMove = tryGetDirectory model.directories dirId
        let dirDestination = tryGetDirectory model.directories parentDirId
        match dirToMove , dirDestination with
            | None , None -> {Fail = Some(NotFound); Success = None}
            | Some dirToMove , Some  dirDest ->
                if (dirToMove.version <> dirId && dirDest.version <> parentVersion )
                then
                    {Fail = Some(Conflict); Success = None}
                else
                    let dirToMovePermission = tryGetRights model.rights dirToMove.parentId
                    let dirDestPermission = tryGetRights model.rights dirDest.parentId
                    
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
        *)                     
    let listFilesModel (model:Model) userId =
       let user = tryGetUser model.users userId
       match user with
       | None -> {Fail = None; Success = Some({directoryVersions = []; fileList = []})}
       | Some user -> {Fail = None; Success = Some({directoryVersions = user.directoryVersions; fileList = user.listFiles})}

    let fileMetaInformationModel (model:Model) userId fileId =
            let fileExists = tryGetFileById model.files fileId
            match fileExists with
                | None -> {Fail = Some(NotFound); Success = None}
                | Some file ->
                 let accessRights = tryGetRights model.rights (string file.metadata.parentId) userId
                 match accessRights with
                       | None -> {Fail = Some(Unauthorized); Success = None}
                       | Some rights ->
                           {Fail = None; Success = Some(file.metadata)}
          
                        
                      
    let dirStructureModel (model:Model) userId =
        let userExists = tryGetUser model.users userId
        match userExists with
            | None -> {Fail = None; Success = Some([])}
            | Some user -> {Fail = None; Success = Some(user.dirStructures)}
        
        
 
    
    let createFileModelWithBug (model:Model) userId (dirId:int) fileName =
        let parentDir = tryGetParent model.directories dirId
        let rights =  tryGetRights model.rights (string dirId) userId
        let fileExists = tryGetFileByNameAndDir model.files fileName dirId
        match rights, parentDir, fileExists with
            | None , Some dir , None -> {Fail= Some(Unauthorized); Success= Some({model with currentFileId = model.currentFileId+1})}
            | None , Some dir , Some file -> {Fail= Some(Conflict); Success= Some({model with currentFileId = model.currentFileId+1})}
            | None, None , None -> {Fail= Some(NotFound); Success= Some({model with currentFileId = model.currentFileId})}
            | Some permission , Some parentDir , Some file -> {Fail= Some(Conflict); Success= Some({model with currentFileId = model.currentFileId+1})}
            | Some permission , Some parentDir , None -> match permission with
                | R -> {Fail= Some(Unauthorized); Success= Some({model with currentFileId = model.currentFileId+1})}
                | NonePermission -> {Fail= Some(Unauthorized); Success= Some({model with currentFileId = model.currentFileId+1})}
                | CRUD -> // has write permission
                    let file = tryGetFileByNameAndDir model.files fileName dirId
                    match file with
                        | Some file -> {Fail= Some(Conflict); Success= Some({model with currentFileId = model.currentFileId+1})}
                        | None ->
                                let newMetadata = {id = model.currentFileId; parentId =  dirId; version = 1; versionChanged = 1; timestamp = "637479675580000000"; name=fileName}
                                let newFile = {content = ""; metadata = newMetadata}
                                let user = model.users |> List.find (fun e -> e.userId = userId) 
                                let dirCheckedOut = user.directoryVersions |> List.tryFind (fun e -> e.id = dirId)
                                match dirCheckedOut with
                                | None ->  {Fail=None; Success = Some({model with  files = newFile::model.files; sutResponse = Some(CreateFileSuccess{id = string model.currentFileId; version = 1; name=fileName; timestamp ="637479675580000000" }) ; currentFileId = model.currentFileId+1}) }    
                                | Some dirCheckedOut ->
                                    
                                    let users =  model.users |> List.map (fun e ->
                                      if (e.userId = userId)
                                      then {e with listFiles = newMetadata::e.listFiles}
                                      else e )
                                    if (model.currentFileId <= 6)  // Bug, by not creating file 
                                    then 
                                    {Fail=None; Success = Some({model with users = users; files = newFile::model.files; sutResponse = Some(CreateFileSuccess{id = string model.currentFileId; version = 1; name=fileName; timestamp ="637479675580000000" }) ; currentFileId = model.currentFileId+1}) }
                                    else
                                        {Fail=None; Success = Some({model with users = users; files = model.files; sutResponse = Some(CreateFileSuccess{id = string model.currentFileId; version = 1; name=fileName; timestamp ="637479675580000000" }) ; currentFileId = model.currentFileId+1}) }
            | _ , _, _ ->  {Fail= Some(NotFound); Success= Some({model with currentFileId = model.currentFileId})}
    
    let createFileModel (model:Model) userId (dirId:int) fileName =
        let parentDir = tryGetParent model.directories dirId
        let rights =  tryGetRights model.rights (string dirId) userId
        let fileExists = tryGetFileByNameAndDir model.files fileName dirId
        match rights, parentDir, fileExists with
            | None , Some dir , None -> {Fail= Some(Unauthorized); Success= Some({model with currentFileId = model.currentFileId+1})}
            | None , Some dir , Some file -> {Fail= Some(Conflict); Success= Some({model with currentFileId = model.currentFileId+1})}
            | None, None , None -> {Fail= Some(NotFound); Success= Some({model with currentFileId = model.currentFileId})}
            | Some permission , Some parentDir , Some file -> {Fail= Some(Conflict); Success= Some({model with currentFileId = model.currentFileId+1})}
            | Some permission , Some parentDir , None -> match permission with
                | R -> {Fail= Some(Unauthorized); Success= Some({model with currentFileId = model.currentFileId+1})}
                | NonePermission -> {Fail= Some(Unauthorized); Success= Some({model with currentFileId = model.currentFileId+1})}
                | CRUD -> // has write permission
                    let file = tryGetFileByNameAndDir model.files fileName dirId
                    match file with
                        | Some file -> {Fail= Some(Conflict); Success= Some({model with currentFileId = model.currentFileId+1})}
                        | None ->
                                let newMetadata = {id = model.currentFileId; parentId =  dirId; version = 1; versionChanged = 1; timestamp = "637479675580000000"; name=fileName}
                                let newFile = {content = ""; metadata = newMetadata}
                                let user = model.users |> List.find (fun e -> e.userId = userId) 
                                let dirCheckedOut = user.directoryVersions |> List.tryFind (fun e -> e.id = dirId)
                                match dirCheckedOut with
                                | None ->  {Fail=None; Success = Some({model with  files = newFile::model.files; sutResponse = Some(CreateFileSuccess{id = string model.currentFileId; version = 1; name=fileName; timestamp ="637479675580000000" }) ; currentFileId = model.currentFileId+1}) }    
                                | Some dirCheckedOut ->
                                    let users =  model.users |> List.map (fun e ->
                                      if (e.userId = userId)
                                      then {e with listFiles = newMetadata::e.listFiles}
                                      else e )
                                    {Fail=None; Success = Some({model with users = users; files = newFile::model.files; sutResponse = Some(CreateFileSuccess{id = string model.currentFileId; version = 1; name=fileName; timestamp ="637479675580000000" }) ; currentFileId = model.currentFileId+1}) } 
            | _ , _, _ ->  {Fail= Some(NotFound); Success= Some({model with currentFileId = model.currentFileId})}
    
    let moveFileModel (model:Model) userId fileId (dirId:int) fileName fileVersion =
        let fileExists = tryGetFileById model.files fileId
        match fileExists with
        | None -> {Fail = Some(NotFound); Success = None}
        | Some file ->
            if file.metadata.version <> fileVersion then {Fail = Some(Conflict); Success = None} else
            let userRights = tryGetRights model.rights (string file.metadata.parentId) userId
            let moveToDirRights = tryGetRights model.rights (string dirId) userId
            match userRights , moveToDirRights with
            | None , None -> {Fail = Some(Unauthorized); Success = None}
            | Some rights , None ->
                let dirExists = tryGetDirectory model.directories dirId
                match dirExists with
                | None  -> {Fail = Some(DirectoryNotFound); Success = None}
                | Some dir -> {Fail = Some(Unauthorized); Success = None}
            | None , Some rights -> {Fail = Some(Unauthorized); Success = None}
            | Some fromDirRights , Some toDirRights -> 
                match fromDirRights , toDirRights with
                | CRUD , CRUD ->
                    let dirExists = tryGetDirectory model.directories dirId
                    match dirExists with
                    | None -> {Fail = Some(DirectoryNotFound); Success = None}
                    | Some dir ->
                        let MoveToSameDirectory = file.metadata.parentId = dirId
                        match MoveToSameDirectory with
                        | true ->
                            let userExists = tryGetUser model.users userId
                            match userExists with
                            | None -> {Fail = Some(Unauthorized); Success = None}
                            | Some user -> 
                                let isDirCheckedOut = tryGetCheckedOutDir user.directoryVersions dirId
                                let fileExistsByDir = tryGetFileByNameAndDir model.files fileName dirId
                                match isDirCheckedOut , fileExistsByDir with
                                | None, Some x -> {Fail = Some(Conflict); Success = None} 
                                | None, None ->
                                    let files = model.files  |> List.map (fun e -> if (e.metadata.id = fileId)  then {e with metadata = {e.metadata with version = e.metadata.version+1; parentId = dirId;name = fileName }}else e )
                                    {Fail = None; Success = Some({model with  files = files; sutResponse = Some(MoveFileSuccess{id = file.metadata.id; version = fileVersion+1; name=fileName})})} 
                                | Some x, None ->  // Moving to an already checked out dir update listfiles
                                         let users = model.users |> List.map (fun e ->
                                                  if (e.userId = userId)
                                                  then {e with listFiles = e.listFiles |> List.map (fun s -> if (s.id = fileId) then {s with version = s.version+1; parentId = dirId; name = fileName} else s) }
                                                  else e )
                                         let files = model.files  |> List.map (fun e ->
                                                  if (e.metadata.id = fileId)
                                                  then {e with metadata = {e.metadata with version = e.metadata.version+1; parentId = dirId;name = fileName }}
                                                  else e )
                                         {Fail = None; Success = Some({model with users = users; files = files; sutResponse = Some(MoveFileSuccess{id = file.metadata.id; version = fileVersion+1; name=fileName})})}
                                | Some(x), Some(y) ->
                                    let fileIsSameName = file.metadata.name = fileName
                                    match fileIsSameName with
                                    | true ->     
                                        let users = model.users |> List.map (fun e ->
                                                      if (e.userId = userId)
                                                      then {e with listFiles = e.listFiles |> List.map (fun s -> if (s.id = fileId) then {s with version = s.version+1; parentId = dirId; name = fileName} else s) }
                                                      else e )
                                        let files = model.files  |> List.map (fun e ->
                                                      if (e.metadata.id = fileId)
                                                      then {e with metadata = {e.metadata with version = e.metadata.version+1; parentId = dirId;name = fileName }}
                                                      else e )
                                        {Fail = None; Success = Some({model with users = users; files = files; sutResponse = Some(MoveFileSuccess{id = file.metadata.id; version = fileVersion+1; name=fileName})})}
                                    | false ->  {Fail = Some(Conflict); Success = None} 
                        | false ->
                            let userExists = tryGetUser model.users userId
                            match userExists with
                            | None -> {Fail = Some(Unauthorized); Success = None}
                            | Some user ->
                                let dirMovingToCheckedOut = tryGetCheckedOutDir user.directoryVersions dirId
                                let dirMovingFromCheckedOut = tryGetCheckedOutDir user.directoryVersions file.metadata.parentId
                                let fileExists = tryGetFileByNameAndDir model.files fileName dirId
                                match dirMovingFromCheckedOut , dirMovingToCheckedOut , fileExists with
                                | None , None , Some file -> {Fail = Some(Conflict); Success = None} //
                                | None , None , None ->  // Update file and not user file list
                                    let files = model.files  |> List.map (fun e ->
                                          if (e.metadata.id = fileId)
                                          then {e with metadata = {e.metadata with version = e.metadata.version+1; parentId = dirId;name = fileName }}
                                          else e )
                                    {Fail = None; Success = Some({model with  files = files; sutResponse = Some(MoveFileSuccess{id = file.metadata.id; version = fileVersion+1; name=fileName})})} 
                                | Some dirFrom , None , Some file -> {Fail = Some(Conflict); Success = None} //
                                | Some dirFrom , None , None -> 
                                     let users = model.users |> List.map (fun e ->
                                              if (e.userId = userId)
                                              then {e with listFiles = e.listFiles |> List.filter (fun e -> e.parentId <> dirFrom.id)  |> List.map (fun s -> if (s.id = fileId) then {s with version = s.version+1; parentId = dirId; name = fileName} else s) }
                                              else e )
                                     let files = model.files  |> List.map (fun e ->
                                              if (e.metadata.id = fileId)
                                              then {e with metadata = {e.metadata with version = e.metadata.version+1; parentId = dirId;name = fileName }}
                                              else e )
                                     {Fail = None; Success = Some({model with users = users; files = files; sutResponse = Some(MoveFileSuccess{id = file.metadata.id; version = fileVersion+1; name=fileName})})}
                                | Some dirFrom , Some toDir , Some file -> {Fail = Some(Conflict); Success = None} //
                                | Some dirFrom , Some toDir , None ->  // Update in list both places
                                         let users = model.users |> List.map (fun e ->
                                                  if (e.userId = userId)
                                                  then {e with listFiles = e.listFiles |> List.map (fun s -> if (s.id = fileId) then {s with version = s.version+1; parentId = dirId; name = fileName} else s) }
                                                  else e )
                                         let files = model.files  |> List.map (fun e ->
                                                  if (e.metadata.id = fileId)
                                                  then {e with metadata = {e.metadata with version = e.metadata.version+1; parentId = dirId;name = fileName }}
                                                  else e )
                                         {Fail = None; Success = Some({model with users = users; files = files; sutResponse = Some(MoveFileSuccess{id = file.metadata.id; version = fileVersion+1; name=fileName})})}
                                                                                       
                                | None , Some toDir , Some file -> {Fail = Some(Conflict); Success = None} 
                                | None , Some toDir , None ->  // Add to user list and update
                                    let fileMeta = {file.metadata with name = fileName; version=file.metadata.version+1; parentId=dirId}
                                    let users = model.users |> List.map (fun e ->
                                              if (e.userId = userId)
                                              then {e with listFiles = fileMeta::e.listFiles}
                                              else e )
                                    let files = model.files  |> List.map (fun e ->
                                              if (e.metadata.id = fileId)
                                              then {e with metadata = {e.metadata with version = e.metadata.version+1; parentId = dirId;name = fileName }}
                                              else e )
                                    {Fail = None; Success = Some({model with users = users; files = files; sutResponse = Some(MoveFileSuccess{id = file.metadata.id; version = fileVersion+1; name=fileName})})} 
                | _ , _ -> {Fail = Some(Unauthorized); Success = None}
                        
                
        
        
        
        
        
        
        
        
        
        
        (*
        
        match fileExists with
            | None -> {Fail = Some(NotFound); Success = None}
            | Some file ->
                if (file.metadata.version <> fileVersion)
                    then
                        {Fail = Some(Conflict); Success = None}
                else
                    let userRights = tryGetRights model.rights (string file.metadata.parentId) userId
                    match userRights with
                        | None -> {Fail = Some(Unauthorized); Success = None}
                        | Some rights ->
                            let moveToDirExists = tryGetDirectory model.directories dirId
                            match moveToDirExists with
                                | None -> {Fail = Some(DirectoryNotFound); Success = None}
                                | Some dir ->
                                    let moveToDirRights = tryGetRights model.rights (string dirId) userId
                                    match moveToDirRights with
                                        | None -> {Fail = Some(Unauthorized); Success = None}
                                        | Some rights ->
                                            match rights with
                                                | R -> {Fail = Some(Unauthorized); Success = None}
                                                | NonePermission -> {Fail = Some(Unauthorized); Success = None}
                                                | CRUD ->
                                                    /// Because that checked out shit 
                                                    let userExists = tryGetUser model.users userId
                                                    match userExists with
                                                        | None -> {Fail = Some(Unauthorized); Success = None}
                                                        | Some user ->
                                                                let fileAlreadyExistsInDir = tryGetFileByNameAndDir model.files fileName dirId
                                                                match fileAlreadyExistsInDir with
                                                                    | Some file ->{Fail = Some(Conflict); Success = None}
                                                                    | None ->
                                                                        let fileAlreadyExists = tryGetFileInSameDirWithSameName model.files fileName dirId
                                                                        match fileAlreadyExists with
                                                                            | Some file -> {Fail = Some(Conflict); Success = None}
                                                                            | None ->
                                                                                let dirMovingToCheckedOut = tryGetCheckedOutDir user.directoryVersions dirId
                                                                                let dirMovingFromCheckedOut = tryGetCheckedOutDir user.directoryVersions file.metadata.parentId
                                                                                match dirMovingFromCheckedOut , dirMovingToCheckedOut with
                                                                                    | None, None -> 
                                                                                        let users = model.users |> List.map (fun e ->
                                                                                                  if (e.userId = userId)
                                                                                                  then {e with listFiles = e.listFiles |> List.map (fun s -> if (s.id = fileId) then {s with version = s.version+1; parentId = dirId; name = fileName} else s) }
                                                                                                  else e )
                                                                                        let files = model.files  |> List.map (fun e ->
                                                                                                  if (e.metadata.id = fileId)
                                                                                                  then {e with metadata = {e.metadata with version = e.metadata.version+1; parentId = dirId;name = fileName }}
                                                                                                  else e )
                                                                                        {Fail = None; Success = Some({model with users = users; files = files; sutResponse = Some(MoveFileSuccess{id = file.metadata.id; version = fileVersion+1; name=fileName})})}
                                                                                    | Some dir, None ->
                                                                                        let users = model.users |> List.map (fun e ->
                                                                                                  if (e.userId = userId)
                                                                                                  then {e with listFiles = e.listFiles |> List.filter (fun e -> e.parentId <> dir.id)  |> List.map (fun s -> if (s.id = fileId) then {s with version = s.version+1; parentId = dirId; name = fileName} else s) }
                                                                                                  else e )
                                                                                        let files = model.files  |> List.map (fun e ->
                                                                                                  if (e.metadata.id = fileId)
                                                                                                  then {e with metadata = {e.metadata with version = e.metadata.version+1; parentId = dirId;name = fileName }}
                                                                                                  else e )
                                                                                        {Fail = None; Success = Some({model with users = users; files = files; sutResponse = Some(MoveFileSuccess{id = file.metadata.id; version = fileVersion+1; name=fileName})})}
                                                                                    | Some dir, Some toDir ->
                                                                                        let users = model.users |> List.map (fun e ->
                                                                                                  if (e.userId = userId)
                                                                                                  then {e with listFiles = e.listFiles |> List.map (fun s -> if (s.id = fileId) then {s with version = s.version+1; parentId = dirId; name = fileName} else s) }
                                                                                                  else e )
                                                                                        let files = model.files  |> List.map (fun e ->
                                                                                                  if (e.metadata.id = fileId)
                                                                                                  then {e with metadata = {e.metadata with version = e.metadata.version+1; parentId = dirId;name = fileName }}
                                                                                                  else e )
                                                                                        {Fail = None; Success = Some({model with users = users; files = files; sutResponse = Some(MoveFileSuccess{id = file.metadata.id; version = fileVersion+1; name=fileName})})}
                                                                                     | None, Some toDir ->
                                                                                        let fileMeta = {file.metadata with name = fileName; version=file.metadata.version+1; parentId=dirId}
                                                                                        let users = model.users |> List.map (fun e ->
                                                                                                  if (e.userId = userId)
                                                                                                  then {e with listFiles = fileMeta::e.listFiles}
                                                                                                  else e )
                                                                                        let files = model.files  |> List.map (fun e ->
                                                                                                  if (e.metadata.id = fileId)
                                                                                                  then {e with metadata = {e.metadata with version = e.metadata.version+1; parentId = dirId;name = fileName }}
                                                                                                  else e )
                                                                                        {Fail = None; Success = Some({model with users = users; files = files; sutResponse = Some(MoveFileSuccess{id = file.metadata.id; version = fileVersion+1; name=fileName})})}
                                            
*)


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
(*
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

   
        (*    
    let moveDirecotrySut (model:Model) userId (dirId:int) dirName parentDirId  =
        let user = model.users |> List.find (fun e -> e.userId = userId)
        let file = user.dirStructures |> List.tryFind (fun e -> e.id = dirId)
        match file with
           | None -> {Fail = Some(Unauthorized(401)); Success = None}
           | Some fi -> API.directoryMove userId (string dirId) (string (int fi.version-1)) dirName parentDirId (string fi.parentId)
           

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
                | Some x -> if (x.create && x.read) then Some {dir = string e.metadata.id; rights =  CRUD; user = e.user } elif (not x.create && x.read) then Some {dir = string e.metadata.id; rights =  R; user = e.user } else Some {dir = string e.metadata.id; rights = NonePermission; user = e.user }
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
     *) 
    let getTestData =
           {
                        users = [
                            {
                                userId = "100"
                                listFiles = [{id = 2; name="README.txt";parentId = 15;version=1;versionChanged=1;timestamp="637479675580000000"}]
                                directoryVersions = [{id=15; version = 1};{id=17;version=1};{id=18;version=1}]
                                dirStructures = [
                                    {id=15;parentId=None;name="";rootPath="/Users/rw/";rootId=15;rootIsDefault=true;version=1}
                                    {id=17;parentId=None;name="";rootPath="/Projects/Project 1/";rootId=17;rootIsDefault=false;version=1}
                                    {id=18;parentId=None;name="";rootPath="/Projects/Project 2/";rootId=18;rootIsDefault=false;version=1}
                                ]
                            };
                              {
                                
                                userId = "101"
                                listFiles = [{id = 3; name="README.txt";parentId = 16;version=1;versionChanged=1;timestamp="637479675580000000"}]
                                directoryVersions = [{id=16; version = 1};{id=17;version=1};{id=18;version=1}]
                                dirStructures = [
                                    {id=16;parentId=None;name="";rootPath="/Users/ro/";rootId=16;rootIsDefault=true;version=1}
                                    {id=17;parentId=None;name="";rootPath="/Projects/Project 1/";rootId=17;rootIsDefault=false;version=1}
                                    {id=18;parentId=None;name="";rootPath="/Projects/Project 2/";rootId=18;rootIsDefault=false;version=1}
                                ]
                            }
                        ]
                        files = [
                            {content = "README.txt located at /Users/rw/README.txt\nOnly USER_ID=100 can access it.\n"
                             metadata = {id = 2; name="README.txt";parentId = 15;version=1;versionChanged=1;timestamp="637479675580000000"}}
                            
                            {
                                content = "README.txt located at /Users/ro/README.txt\nOnly USER_ID=101 can access it.\n"
                                metadata = {id = 3; name="README.txt";parentId = 16;version=1;versionChanged=1;timestamp="637479675580000000"}
                            }
                            {
                                content = "INTRO.txt located at /Users/Shared files/INTRO.txt\n USER_ID=100 (rw) can read and write content to the file, but USER_ID=101 (ro) can only read it. USER_ID=102 (none) has no access it."
                                metadata = {id = 4; name="INTRO.txt";parentId = 9;version=1;versionChanged=1;timestamp="637479675580000000"}
                            }
                            
                        ]
                        
                        
                        directories = [
                            {id=1;parentId=None;name="";rootPath="/Users/rw/";rootId=15;rootIsDefault=true;version=1};
                                {id=10;parentId=Some(1);name="";rootPath="/Users/rw/";rootId=15;rootIsDefault=true;version=1};
                                {id=13;parentId=Some(1);name="";rootPath="/Users/rw/";rootId=15;rootIsDefault=true;version=1};
                                    {id=21;parentId=Some(13);name="";rootPath="/Users/rw/";rootId=15;rootIsDefault=true;version=1};
                                {id=3;parentId=Some(1);name="";rootPath="/Users/rw/";rootId=15;rootIsDefault=true;version=1};
                                {id=8;parentId=Some(1);name="";rootPath="/Users/rw/";rootId=15;rootIsDefault=true;version=1};
                                {id=2;parentId=Some(1);name="";rootPath="/Users/rw/";rootId=15;rootIsDefault=true;version=1};
                                    {id=17;parentId=Some(2);name="";rootPath="/Projects/Project 1/";rootId=17;rootIsDefault=false;version=1};
                                    {id=18;parentId=Some(2);name="";rootPath="/Projects/Project 2/";rootId=18;rootIsDefault=false;version=1};
                                {id=4;parentId=Some(1);name="";rootPath="/Users/rw/";rootId=15;rootIsDefault=true;version=1};
                                    {id=5;parentId=Some(4);name="";rootPath="/Users/rw/";rootId=15;rootIsDefault=true;version=1};
                                        {id=6;parentId=Some(5);name="";rootPath="/Users/rw/";rootId=15;rootIsDefault=true;version=1};
                                        {id=7;parentId=Some(5);name="";rootPath="/Users/rw/";rootId=15;rootIsDefault=true;version=1};
                                {id=12;parentId=Some(1);name="";rootPath="/Users/rw/";rootId=15;rootIsDefault=true;version=1};
                                {id=9;parentId=Some(1);name="";rootPath="/Users/rw/";rootId=15;rootIsDefault=true;version=1};
                                    {id=20;parentId=Some(9);name="";rootPath="/Users/rw/";rootId=15;rootIsDefault=true;version=1};
                                {id=11;parentId=Some(1);name="";rootPath="/Users/rw/";rootId=15;rootIsDefault=true;version=1};
                                {id=14;parentId=Some(1);name="";rootPath="/Users/rw/";rootId=15;rootIsDefault=true;version=1};
                                    {id=19;parentId=Some(14);name="";rootPath="/Users/rw/";rootId=15;rootIsDefault=true;version=1};
                                    {id=16;parentId=Some(14);name="";rootPath="/Users/rw/";rootId=15;rootIsDefault=true;version=1};
                                    {id=15;parentId=Some(14);name="";rootPath="/Users/rw/";rootId=15;rootIsDefault=true;version=1};

                        ]
                        
                        sutResponse = None
                        currentFileId = 5
                        deletedFileVersion = 0
                        currentDirId = 0
                        currentUpdatedFile = 0
                        currentUpdatedDirId = 0
                        rights = [{dir = "17";rights = R; user= "101"}; {dir = "18";rights = R; user = "101"}; {dir = "9"; rights= R; user = "101"}; {dir = "16"; rights=R;user="101"}; {dir = "20";rights=R;user="101"}
                                  {dir = "17";rights = CRUD; user= "100"}; {dir = "18";rights = CRUD; user = "100"}; {dir = "9"; rights= CRUD; user = "100"}; {dir = "15"; rights=CRUD;user="100"};{dir = "20";rights=CRUD;user="100"}]
                    }