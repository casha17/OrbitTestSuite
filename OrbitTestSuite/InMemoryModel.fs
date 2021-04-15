namespace OrbitTestSuite.InMemoryModel

open System
open OrbitTestSuite.Models
open OrbitTestSuite.API
open OrbitTestSuite.Models.APIModels

open HttpFs.Client

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

    let rec listLength list =
        match list with
        | [] -> 0
        | x::xs -> 1 + listLength xs

    type inMemoryModel() =
        let mutable (files:List<inMemoryFile>) = []
        let mutable (directoryVersions:List<directoryVersion>) = []
        let mutable (directories:List<directoryStructure>) = [] 
        let mutable (user:user) = {
            id = -1
            name = ""
            initials = ""
        }
        
        // Store the intermediate result of the listFiles endpoint
        let mutable (filesResponse:API.BaseResponse<listFilesResponse>) = 
            {
                data = Unchecked.defaultof<listFilesResponse>
                response = Unchecked.defaultof<Response>
            }
            
        let mutable (metadataResponseByFileId: API.BaseResponse<metadata>) =
            {
                data = Unchecked.defaultof<metadata>
                response = Unchecked.defaultof<Response>
            }
        
        member __.GetRandomFileId seed =
            if files.IsEmpty then -1
            else
                let random = Random(seed)
                files.[(random.Next(0, listLength files))].metadata.id
                
        member __.GetInMemoryFilesMetadataByFiledId fileId =
            files |> List.filter (fun x -> x.metadata.id = fileId)
        
        member __.GetFiles = files
        member __.GetDirectories = directories
        member __.GetUser = user
        member __.GetDirectoryVersions = directoryVersions
        member __.appendFiles newFiles = files <- newFiles
        member __.appendDirectories newDirectories = directories <- newDirectories
        member __.setUser newUser = user <- newUser
        member __.setDirectoryVersions newVersions = directoryVersions <- newVersions
        member __.ToString = printf "Files: \n\n %A  Directories: \n\n %A User: \n\n %A" files directories user
        member __.GetFilesResponse = filesResponse
        member __.GetFilesAPI userId = filesResponse <- API.listFiles userId
        member __.GetFilesMetadataByFileId userId fileId = metadataResponseByFileId <- API.fileMetaInformationByFileId userId fileId
        member __.GetFilesMetadataByFiledIdResponse = metadataResponseByFileId
