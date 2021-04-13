namespace OrbitTestSuite.Models

module APIModels =
    
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




