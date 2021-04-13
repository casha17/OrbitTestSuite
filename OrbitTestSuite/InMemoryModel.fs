namespace OrbitTestSuite.InMemoryModel

open OrbitTestSuite.Models
open OrbitTestSuite.API
open OrbitTestSuite.Models.APIModels
module inMemoryModels = 


    type user = {
        id: int
        name: string
        initials: string
    }

    
    type inMemoryFile = {
        content: string
        metadata: APIModels.metadata
    }

    
    

    type inMemoryModel() =
        let mutable (files:List<inMemoryFile>) = []
        let mutable (directoryVersions:List<APIModels.directoryVersion>) = []
        let mutable (directories:List<APIModels.directoryStructure>) = [] 
        let mutable (user:user) = {
            id = -1
            name = ""
            initials = ""
        }
        
        let mutable (getFilesResponse:BaseResponse<listFilesResponse>)
        
        
        
        member __.GetFiles = files
        member __.GetDirectories = directories
        member __.GetUser = user
        member __.GetDirectoryVersions = directoryVersions
        member __.appendFiles newFiles = files <- newFiles
        member __.appendDirectories newDirectories = directories <- newDirectories
        member __.setUser newUser = user <- newUser
        member __.setDirectoryVersions newVersions = directoryVersions <- newVersions
        member __.ToString = printf "Files: \n\n %A  Directories: \n\n %A User: \n\n %A" files directories user
        member __.GetFilesAPI userId = getFilesResponse <- API.listFiles userId
