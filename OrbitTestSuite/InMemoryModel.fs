namespace OrbitTestSuite.InMemoryModel

open System
open OrbitTestSuite.Models
open OrbitTestSuite.API
open OrbitTestSuite.Models.APIModels

open HttpFs.Client

module inMemoryModels = 


    type file =
            {
                content: string
                fileId:string
                fileVersion: int
            }
        type user =
            {
                files: file list
                userId: string
            }
        type inMemory =
            {
                users: user list 
            }
        
        
    
    type inMemoryFile = {
        content: string
        metadata: APIModels.metadata
    }

    let rec listLength list =
        match list with
        | [] -> 0
        | x::xs -> 1 + listLength xs

    let rec appendToFile (list:List<inMemoryFile>) fileId content = match list with
        | [] -> []
        | el::els ->
            if (el.metadata.id) = fileId
                then {el with content = content; metadata = {id = el.metadata.id; version = el.metadata.version+1; versionChanged = el.metadata.versionChanged+1; name = el.metadata.name; timestamp = el.metadata.timestamp; parentId = el.metadata.parentId; } }::appendToFile els fileId content
            else appendToFile els fileId content
   
    
    let formatList (files:list<inMemoryFile>) =
        let s = System.Text.StringBuilder()
        files |> List.map (fun x -> s.Append(x.content))
        s
    let rec getCurrentFileVersion list fileId = match list with 
        | [] -> -1
        | el::els -> if (el.metadata.id = fileId) then el.metadata.version else getCurrentFileVersion els fileId

    let rec getCurrentFileContent list fileId = match list with 
        | [] -> ""
        | el::els -> if (el.metadata.id = fileId) then el.content else getCurrentFileContent els fileId
    //[<StructuredFormatDisplay("file: {files}")>]
    
    
    type inMemoryModel() =
       
        let mutable (files:List<inMemoryFile>) = []
        let mutable (directoryVersions:List<directoryVersion>) = []
        let mutable (directories:List<directoryStructure>) = [] 
   
        
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
        //member __.GetUser = user
        member __.GetDirectoryVersions = directoryVersions
        member __.appendFiles newFiles = files <- newFiles
        member __.appendDirectories newDirectories = directories <- newDirectories
       // member __.setUser newUser = user <- newUser
        member __.setDirectoryVersions newVersions = directoryVersions <- newVersions
       // member __.ToString = printf "Files: \n\n %A  Directories: \n\n %A User: \n\n %A" files directories user
        member __.GetFilesResponse = filesResponse
       // member __.GetFilesAPI userId = filesResponse <- API.listFiles userId
        //member __.GetFilesMetadataByFileId userId fileId = metadataResponseByFileId <- API.fileMetaInformationByFileId userId fileId
        member __.GetFilesMetadataByFiledIdResponse = metadataResponseByFileId
        member __.WriteToLocalFile userId newContent = files <- appendToFile files userId newContent

        member __.getCurrentFileVersion fileId = getCurrentFileVersion files fileId
        member __.getCurrentFileContent fileId = getCurrentFileContent files fileId
        override this.ToString() = sprintf "InMemoryFiles: %A" (formatList files)
        
    [<StructuredFormatDisplay("test")>]
    type apiModel(?init:int) =
          

       
        let mutable fileVersion =   0  
            
        member __.Initilize =
            let fileVersion = API.listFiles "100"
            let s = fileVersion.data.fileList |> Seq.map (fun e -> {users = [{files = [{content = ""; fileId= "2"; fileVersion = 1}]; userId = "2"}]} )
            s
      
        member __.GetCurrentfileVersion = fileVersion
        member __.PostFile content userId id fileId version timestamp =
           let s = (API.fileupload content userId id fileId version timestamp)
           fileVersion <- s.data.version
           s
           
         member __.ToString = "ss"
           
