namespace OrbitTestSuite.Models

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
        id: int
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
        id: int
        version: int
        parentId: int
        newVersion: directoryVersion list
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




module Model =
    type File =
            {
                content: string
                fileId:string
                fileVersion: int
            }
    
        type Modelmetadata = {
            id: string
            name: string
            parentId: string
            version: int
            versionChanged: int
            timeStamp: string
        }
        type User =
            {
                files: File list
                userId: string
                directoryVersions: ApiResponseModels.directoryVersion list
                fileList: ApiResponseModels.metadata list
            }
        type Model =
            {
                users: User list 
            }
        type TestResponse = {
            fail: bool
            content: Model option
        }
        
        type ErrorCodes =
            NoUserIdSupplied of int
            | FileNotFound of int
            
            
        type listFileResponse<'a> = {
            Fail: ErrorCodes option
            Success : 'a option
        }
    