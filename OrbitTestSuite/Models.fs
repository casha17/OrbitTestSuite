namespace OrbitTestSuite.Models

open HttpFs.Client
open System

module ApiResponseModels =
    
    type C = C of int 
    

    type metadata = {
        id: int
        version: int
        versionChanged: int
        name: string
        parentId: int
        timestamp: string
    }

    type file = {
        content: string
    }
    let fileMapper a = {content = a}

    type directoryVersion = {
        id: int
        version: int
    }

    type listFilesResponse = {
        directoryVersions: directoryVersion list
        fileList: metadata list
    } 

    type directoryStructure = {
        id: int
        parentId: int option
        name: string
        rootPath: string
        rootId: int
        rootIsDefault: bool
        version: int
    }

    type createFile = {
        id: string
        version: int
        name: string
        timestamp: string
    }

    type moveFile = {
        id: int
        version: int
        name: string
    }

    type updateFileTimeStamp = {
        timestamp: string
        version: int
    }

    type fileUpload = {
        id: int
        version: int
        timestamp: string
    }

    type fileLock = {
        lockers: string list
    }

    type directoryCreate = {
        name: string
        id: string
        version: int
        parentId: int
        newVersions: directoryVersion list
    }

    type directoryMove = {
        success: bool
        newVersion: directoryVersion list
    }

    type fileDelete = {
        success: bool
    }

    type directoryDelete = {
        success: bool
    }
    
    type permissionResponse = {
        create: bool
        read: bool
        update: bool
        delete: bool
    }
    
    type parentResponse = {
        id: int
    }
    type DirMetaResponse = {
        id: int
        name: string
        path: string
        version: int
        __permissions: permissionResponse option
        parent: parentResponse
        is_checked_out: bool
        is_default: bool
    }


    type DirMetaResponseWithUser = {
        metadata: DirMetaResponse
        user: string
    }

     type userListDir = {
        user: string
        directories: int
    }
    
   

module Model =
    

    type File =
            {
                content: string
                metadata: ApiResponseModels.metadata
            }
    

        type Modelmetadata = {
            id: string
            name: string
            parentId: string
            version: int
            versionChanged: int
            timeStamp: string
        }
        
        type permission =
            | CRUD
            | R
            | NonePermission
            
            
         type dirAndRights = {
            dir: string
            rights: permission 
            user: string
        }
        type User =
            {
                (*userFiles: Map<string,permission>*)
                userId: string
                directoryVersions: ApiResponseModels.directoryVersion list
                listFiles: ApiResponseModels.metadata list
                dirStructures: ApiResponseModels.directoryStructure list
                
            }
            
            type ResponseCode =
                | CreateFileSuccess of ApiResponseModels.createFile
                | UploadFileSuccess of ApiResponseModels.fileUpload
                | NotFound
                | Conflict
                | Unauthorized
                | DirectoryNotFound
                | MoveFileSuccess of ApiResponseModels.moveFile
                | NoUserIdSupplied of int
                | FileNotFound of int
                | MissingUserId of int
                | FileAlreadyExist of int
                | ParentDirectoryNotFound of int
                | InvalidFileName of int
                | FilePathTooLong of int
                
        

           
            
        [<StructuredFormatDisplay("{DisplayValue}")>]
        type Model =
            {
                users: User list
                files: File list
                currentFileId: int
                deletedFileVersion: int
                currentUpdatedFile:int
                currentDirId:int
                currentUpdatedDirId: int
                rights: dirAndRights list
                sutResponse : ResponseCode option
                directories: ApiResponseModels.directoryStructure list
            }
            
        type Model with
        
            override t.ToString() =
                (*
                let files = (t.files |> List.map (fun e -> "\n\t id:" + (string e.metadata.id) + " parentId:" +  (string e.metadata.parentId) + " version:" + (string e.metadata.version) + " content:"  + e.content.Replace("\n" , "") )) |> List.fold (+) "" 
                sprintf "\nfiles:%s - currentFileId:%i  \n\n" files  t.currentFileId
                *)
                let files = (t.files |> List.map (fun e -> "[id:" + (string e.metadata.id) + " version:" + (string e.metadata.version) + "]"  )) |> List.fold (+) ""
                sprintf "files:%s - currentFileId:%i" files  t.currentFileId
            member t.DisplayValue = t.ToString()
                
        type TestResponse = {
            fail: bool
            content: Model option
        }
        
      
            
        type listFileResponse<'a> = {
            Fail: ResponseCode option
            Success : 'a option
        }

        