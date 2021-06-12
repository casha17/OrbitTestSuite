namespace OrbitTestSuite.TestSuite




open System
open System.Threading.Tasks
open Fake.Core
open System.Threading
open FsCheck

open FsCheck.Arb
open FsCheck.Experimental
open Microsoft.VisualBasic.CompilerServices
open OrbitTestSuite.DockerIntegration
open OrbitTestSuite.DockerIntegration.Docker
open OrbitTestSuite.Models.Model
open OrbitTestSuite.Models.ApiResponseModels
open OrbitTestSuite.API
open OrbitTestSuite.Utilities
open OrbitTestSuite.InMemoryModel

module testSuite =
    
    exception UploadFileException
    exception MoveFileException
    exception DeleteFileException
    exception CreateDirectoryException
    
    let createFileIdGenerator model =
         let mutable fileIds = Utilities.getAllFileIds model.files
         if (fileIds.Length = 0) then
            Gen.frequency [ (1 ,Arb.generate<int>)]
         else
            Gen.frequency [(2 ,Gen.elements fileIds); (1 ,Arb.generate<int>);]
    let createDirectoryGen model =
        let mutable dirIds = Utilities.getALlDirIds model
        if (dirIds.Length = 0) then
           Gen.frequency [ (1 ,Gen.choose(22,50)); ((9 ,Gen.choose(15,16)))]
        else
            Gen.frequency [(5 ,Gen.elements dirIds); (1 ,Gen.choose(22,50)); ((9 ,Gen.choose(15,16)))]
    let createFileVersionGen model =
        let mutable fileVersions = Utilities.getAllFileVersions model.files
        if (fileVersions.Length = 0) then 
            Gen.frequency [ (1 ,Gen.choose(1,50))]
        else
            Gen.frequency [(10 ,Gen.elements fileVersions); (1 ,Gen.choose(1,50))]
                
    let orbitAPI =
        

        let uploadFile content userId (fileId:int) fileVersion timestamp = 
            { new Operation<apiModel,Model>() with
                member __.Run model =
                    let modelResponse = Utilities.uploadFileModel model content userId  fileId fileVersion timestamp
                    match modelResponse.Fail , modelResponse.Success with
                        | None , Some newModel ->  newModel
                        | Some error , None-> {model with sutResponse = Some(error)}
                        | _ , _ -> raise UploadFileException

                member op.Check (systemUnderTest,model) =
                    let response = API.fileUpload content userId (string fileId) (string fileVersion) timestamp
                    match response.Fail , response.Success, model.sutResponse with
                        | None , Some sut , Some model -> match model with
                            | UploadFileSuccess m ->
                            let res = sut = m
                            res.ToProperty |@ sprintf "Response code after create different:\n Model:%A SUT:%A" m sut 
                            | _ -> false.ToProperty |@ sprintf "Should not happen that model returns another response code that UploadFileSuccess"
                        | Some sutError , None , Some modelError -> match sutError , modelError with
                           | NotFound , NotFound -> true.ToProperty |@ sprintf ""
                           | Unauthorized , Unauthorized -> true.ToProperty |@ sprintf ""
                           | Conflict , Conflict -> true.ToProperty |@ sprintf ""
                           | _ , _ -> false.ToProperty |@ sprintf "Different error response after create: \n SUT:%A MODEL:%A" sutError modelError
                        | _ , _ , _ -> false.ToProperty |@ sprintf "Model or Sut didnt return success or error"
                override __.ToString() = sprintf "uploadFile: content: %s toUser:%s fileId:%i fileVersion:%i" content userId fileId fileVersion
            }

        let downloadFile userId (fileId:int) = 
            { new Operation<apiModel,Model>() with
                member __.Run model = model
                member op.Check (sut,model) =
                    let modelResponse = Utilities.downloadFileModel model userId  fileId
                    let sutResponse = API.downloadFile userId (string fileId)
                    match sutResponse.Fail , sutResponse.Success , modelResponse.Fail , modelResponse.Success with
                        | None , Some c , None , Some m ->
                            let s = (c = m)
                            s.ToProperty // Is the file content the same
                        | Some sut , None , Some model , None -> 
                            match sut , model with
                                // Is the error from the SUT and the model the same
                                | Unauthorized , Unauthorized  -> true.ToProperty
                                | NotFound , NotFound -> true.ToProperty
                                | _  , _  -> false.ToProperty
                        | _ , _ ,_ ,_ -> false.ToProperty
                    |@ sprintf "Download file:%i for user: %s " fileId userId 
                   
                override __.ToString() = sprintf "Downloaded file:%i for user: %s " fileId userId }
     
        let listFiles userId = 
            { new Operation<apiModel,Model>() with
                member __.Run model = model
                member op.Check (sut,model) =
                    let modelResponse = Utilities.listFilesModel model userId
                    let sutResponse = API.listFiles userId
                    match sutResponse.Fail , sutResponse.Success , modelResponse.Fail , modelResponse.Success with
                        | None , Some s , None , Some m ->
                        let g = s.fileList |> List.sortBy (fun e -> e.id)
                        let x = m.fileList |> List.sortBy (fun e -> e.id)
                        let v = g = x
                        v.ToProperty
                        | Some sut , None , Some model , None -> 
                            match sut , model with
                                | NoUserIdSupplied x , NoUserIdSupplied y  -> true.ToProperty
                                | _  , _  -> false.ToProperty
                        | _ , _ , _ , _ -> printf "should never reach this state"; false.ToProperty
                    |@ sprintf "list files SUT: %A - MODEL: %A " sutResponse.Success modelResponse.Success
                override __.ToString() = sprintf "listfiles for user=%s" userId
            }

        let fileMetaInformation userId (fileId:int) = 
            { new Operation<apiModel,Model>() with
                member __.Run model =
                    printf "%i," fileId
                    model
                member op.Check (sut,model) =
                    let modelResponse = Utilities.fileMetaInformationModel model userId   fileId
                    let sutResponse = API.fileMetaInformationByFileId userId (string fileId)
                    match sutResponse.Fail , sutResponse.Success , modelResponse.Fail , modelResponse.Success with
                        | None , Some s , None , Some m ->
                            let v = s = m
                            v.ToProperty() |@ sprintf "fileMeta different model:  %A SUT: %A" m s
                        | Some sut , None , Some model , None -> 
                            match sut , model with
                                | NotFound , NotFound  -> true.ToProperty()
                                | Unauthorized , Unauthorized -> true.ToProperty()
                                | _  , _  -> false.ToProperty()
                        | _ , _ , _ , _ -> printf "should never reach this state"; false.ToProperty()
                override __.ToString() = sprintf "fileMetaInformation for user=%s fileId=%i" userId fileId
            }
                   
        let dirStructure userId  = 
            { new Operation<apiModel,Model>() with
                member __.Run model = model
                member op.Check (sut,model) =
                    let modelResponse = Utilities.dirStructureModel model userId  
                    let sutResponse = API.directoryStructure userId 
                    match sutResponse.Fail , sutResponse.Success , modelResponse.Fail , modelResponse.Success with
                        | None , Some s , None , Some m ->  
                        let g = s |> List.sortBy (fun e -> e.id)
                        let x = m |> List.sortBy (fun e -> e.id)
                        let v = g = x
                        v.ToProperty |@ sprintf "SUT:%A - MODEL:%A" g x 
                        | Some sut , None , Some model , None -> 
                            match sut , model with
                                | _  , _  -> false.ToProperty()
                        | _ , _ , _ , _ -> printf "should never reach this state"; false.ToProperty()
                    |@ sprintf "Directory structure: "
                override __.ToString() = sprintf "DirStructure for user=%s " userId 
            }
       
        let createFile (userId:string) dirId fileName timestamp  = 
            { new Operation<apiModel,Model>() with
                member __.Run model =
                    let modelResponse = Utilities.createFileModel model userId dirId fileName
                    match modelResponse.Fail , modelResponse.Success with
                        | None , Some newModel  ->   newModel
                        | Some error , Some newModel -> {newModel with sutResponse = Some(error) }
                member op.Check (sut,model) =
                    let sutResponse = API.createFile userId (string dirId) fileName timestamp
                    match sutResponse.Fail,sutResponse.Success , model.sutResponse with
                        | None , Some sut , Some model  -> match model with
                            | CreateFileSuccess m ->
                                let s = (sut = m)
                                s.ToProperty |@ sprintf "SUT:%A - MODEL:%A - %A" sut m (sut=m)
                            | _ -> false.ToProperty |@ sprintf "false: SUT:%A - model:%A" sut  model
                         | Some sutError , None , Some modelError -> match sutError , modelError with
                            | Unauthorized , Unauthorized -> true.ToProperty |@ "true"
                            | Conflict , Conflict -> true.ToProperty |@ "true"
                            | NotFound , NotFound -> true.ToProperty |@ "true"
                           
                            | _ , _ -> false.ToProperty |@ sprintf "Test failed: SUT:%A - MODEL:%A" sutError modelError
                        | _ , _ , _ -> false.ToProperty |@ sprintf "Test failed: SUT error :%A - SUT success:%A - MODEL:%A" sutResponse.Fail sutResponse.Success model.sutResponse 
                                
                    
                override __.ToString() = sprintf "Create file for user=%s dirId=%i filename=%s" userId dirId fileName 
            }
           
        let movefile (userId:string) fileId dirId newFileName fileVersion  = 
            { new Operation<apiModel,Model>() with
                member __.Run model =
                    let modelResponse = Utilities.moveFileModel model userId  fileId dirId newFileName fileVersion
                    match modelResponse.Fail , modelResponse.Success with
                        | None , Some newModel ->  newModel
                        | Some error , None -> {model with sutResponse = Some(error)}
                        | _ , _ -> raise MoveFileException
                member op.Check (sut,model) =
                    let response = API.fileMove userId (string fileId) (string fileVersion) (string dirId) newFileName 
                    match response.Fail , response.Success, model.sutResponse with
                        | None , Some sut , Some model -> match model with
                            | MoveFileSuccess m ->
                            let res = sut = m
                            res.ToProperty |@ sprintf "Response code after create different:\n Model:%A SUT:%A" m sut 
                            | _ -> false.ToProperty |@ sprintf "Should not happen that model returns another response code that MoveFileSuccess"
                        | Some sutError , None , Some modelError -> match sutError , modelError with
                           | NotFound , NotFound -> true.ToProperty |@ sprintf ""
                           | Unauthorized , Unauthorized -> true.ToProperty |@ sprintf ""
                           | Conflict , Conflict -> true.ToProperty |@ sprintf ""
                           | DirectoryNotFound , DirectoryNotFound -> true.ToProperty |@ sprintf ""
                           | InternalServerError , InternalServerError -> true.ToProperty |@ sprintf ""
                           | _ , _ -> false.ToProperty |@ sprintf "Different error response after move: \n SUT:%A MODEL:%A" sutError modelError
                        | _ , _ , _ -> false.ToProperty |@ sprintf "Model or Sut didnt return success or error"
                override __.ToString() = sprintf "Move file for user=%s dirId=%i fileId=%i filename=%s version:=%i" userId dirId fileId  newFileName fileVersion
            }    
                
         
        let updateFileTimeStamp (userId:string) fileId fileVersion   = 
            { new Operation<apiModel,Model>() with
                member __.Run model =
                    let modelResponse = Utilities.updateFileTimestampModel model userId fileId fileVersion "637479675580000000" 
                    match modelResponse.Fail , modelResponse.Success with
                        | None , Some newModel  -> newModel
                        | Some error , None  -> {model with sutResponse = Some(error)}
                        | _ , _ -> model
                member op.Check (sut,model) =
                    let sutResponse = API.updateFileTimestamp userId (string fileId) (string fileVersion) "637479675580000000"  
                    match sutResponse.Fail , sutResponse.Success , model.sutResponse  with
                        | None , Some sut , Some model -> match model with 
                            | UpdateTimestampSuccess m ->
                                let res = sut = m
                                res.ToProperty()
                            | _ -> false.ToProperty()
                        | Some sutError , None , Some modelError -> match sutError , modelError with
                            | NotFound , NotFound -> true.ToProperty()
                            | Conflict , Conflict -> true.ToProperty()
                            | Unauthorized , Unauthorized -> true.ToProperty()
                            | _ , _ -> false.ToProperty()
                        | _ , _ , _ -> false.ToProperty()
                    |@ sprintf "UpdateTimestamp: "
                override __.ToString() = sprintf "update timestamp file for user=%s fileId=%i version=%i " userId  fileId  fileVersion
            }
        
     
        let fileDelete (userId:string) fileId fileVersion   = 
            { new Operation<apiModel,Model>() with
                member __.Run model =
                    let modelResponse = Utilities.fileDeleteModel model userId fileId fileVersion
                    match modelResponse.Fail , modelResponse.Success with
                        | None , Some newModel  -> newModel
                        | Some error , None  -> {model with sutResponse = Some(error)}
                        | _ , _ -> raise DeleteFileException
                member op.Check (sut,model) =
                    let sutResponse = API.fileDelete userId (string fileId) (string fileVersion)   
                    match sutResponse.Fail , sutResponse.Success , model.sutResponse  with
                    | None , Some sut , Some model -> match model with 
                            | DeleteFileSuccess m ->
                                let res = sut = m
                                res.ToProperty()
                            | _ -> false.ToProperty()
                    | Some sutError , None , Some modelError -> match sutError , modelError with
                            | NotFound , NotFound -> true.ToProperty()
                            | Conflict , Conflict -> true.ToProperty()
                            | Unauthorized , Unauthorized -> true.ToProperty()
                            | _ , _ -> false.ToProperty()
                    | _ , _ , _ -> false.ToProperty()
                    |@ sprintf "Delete File:  "
                override __.ToString() = sprintf "Delete file for user=%s fileId=%i fileVersion =%i" userId  fileId  fileVersion
            }
       
        let createDirectory (userId:string) dirId dirName dirVersion  = 
            { new Operation<apiModel,Model>() with
                member __.Run model =
                    let modelResponse = Utilities.createDirectoryModel model userId dirId dirName dirVersion
                    match modelResponse.Fail , modelResponse.Success with
                        | None , Some newModel  ->  newModel
                        | Some error , Some newModel  -> {newModel with sutResponse = Some(error)} 
                        | _ , _ -> raise CreateDirectoryException
                member op.Check (sut,model) =
                    let sutResponse = API.directoryCreate userId (string dirId) dirName (string dirVersion)
                    match sutResponse.Fail , sutResponse.Success , model.sutResponse  with
                        | None , Some sut , Some model  -> match model with
                            | CreateDirectorySucces m ->
                                let s = (sut = m)
                                s.ToProperty |@ sprintf "%A" (sut=m)
                            | _ -> false.ToProperty |@ " false"
                        | Some sutError , None , Some modelError -> match sutError , modelError with
                            | Unauthorized , Unauthorized -> true.ToProperty |@ "true"
                            | Conflict , Conflict -> true.ToProperty |@ "true"
                            | NotFound , NotFound -> true.ToProperty |@ "true"
                            | InternalServerError , InternalServerError -> true.ToProperty |@ "true"             ///// ERROR FOUND
                            | _ , _ ->  false.ToProperty |@ sprintf "test failed: Sut response:%A - modelresponse :%A" sutError modelError 
                        | _ , _ , _ ->printf "SUT:%A - MODEL:%A" sut model; false.ToProperty |@ "false"
                override __.ToString() = sprintf "Create direcotry for user=%s dirId=%i dirName=%s version=%i" userId dirId dirName dirVersion 
            }
               (* 
        let directoryMove (userId:string) dirId newDirName parentDirId  = 
            { new Operation<apiModel,Model>() with
                member __.Run model =
                    let modelResponse = Utilities.moveDirectoryModel model userId dirId newDirName parentDirId
                    match modelResponse.Fail , modelResponse.Success with
                        | None , Some newModel  ->  currentUpdatedDir <- newModel.currentUpdatedDirId; newModel
                        | Some error , Some newModel  -> currentUpdatedDir <- newModel.currentUpdatedDirId; newModel
                        | _ , _ -> model
                member op.Check (sut,model) =
                    let sutResponse = Utilities.moveDirecotrySut model userId dirId newDirName parentDirId
                    match sutResponse.Fail , sutResponse.Success  with
                        | _ , _t  ->  true.ToProperty
                    |@ sprintf "moveFile: "
                override __.ToString() = sprintf "Move file for user=%s dirId=%i " userId dirId 
            }
            *) 
        let create = 
            { new Setup<apiModel,Model>() with
                member __.Actual() =
                    Thread.Sleep 900
                    let r =  Docker.executeShellCommand "docker stop orbit"  |> Async.RunSynchronously
                    Thread.Sleep 200
                    let r =  Docker.executeShellCommand "docker run -d --name orbit --rm -p8085:8085 -eCLICOLOR_FORCE=2 cr.orbit.dev/sdu/filesync-server:latest"  |> Async.RunSynchronously
                    Thread.Sleep 6000
                    apiModel()
                member __.Model() =
                    let model = {
                        users = [
                            {
                                userId = "100"
                                listFiles = [{id = 2; name="README.txt";parentId = 15;version=1;versionChanged=1;timestamp="637479675580000000"}]
                                directoryVersions = [{id=15; version = 1};{id=17;version=1};{id=18;version=1}]
                                dirStructures = [
                                    {id=15;parentId=None;name="";rootPath="/Users/rw/";rootId=15;version=1}
                                    {id=17;parentId=None;name="";rootPath="/Projects/Project 1/";rootId=17;version=1}
                                    {id=18;parentId=None;name="";rootPath="/Projects/Project 2/";rootId=18;version=1}
                                ]
                            };
                              {
                                
                                userId = "101"
                                listFiles = [{id = 3; name="README.txt";parentId = 16;version=1;versionChanged=1;timestamp="637479675580000000"}]
                                directoryVersions = [{id=16; version = 1};{id=17;version=1};{id=18;version=1}]
                                dirStructures = [
                                    {id=16;parentId=None;name="";rootPath="/Users/ro/";rootId=16;version=1}
                                    {id=17;parentId=None;name="";rootPath="/Projects/Project 1/";rootId=17;version=1}
                                    {id=18;parentId=None;name="";rootPath="/Projects/Project 2/";rootId=18;version=1}
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
                            {id=1;parentId=None;name="";rootPath="/Users/rw/";rootId=15;version=1};
                                {id=10;parentId=Some(1);name="";rootPath="/Users/rw/";rootId=15;version=1};
                                {id=13;parentId=Some(1);name="";rootPath="/Users/rw/";rootId=15;version=1};
                                    {id=21;parentId=Some(13);name="";rootPath="/Users/rw/";rootId=15;version=1};
                                {id=3;parentId=Some(1);name="";rootPath="/Users/rw/";rootId=15;version=1};
                                {id=8;parentId=Some(1);name="";rootPath="/Users/rw/";rootId=15;version=1};
                                {id=2;parentId=Some(1);name="";rootPath="/Users/rw/";rootId=15;version=1};
                                    {id=17;parentId=Some(2);name="";rootPath="/Projects/Project 1/";rootId=17;version=1};
                                    {id=18;parentId=Some(2);name="";rootPath="/Projects/Project 2/";rootId=18;version=1};
                                {id=4;parentId=Some(1);name="";rootPath="/Users/rw/";rootId=15;version=1};
                                    {id=5;parentId=Some(4);name="";rootPath="/Users/rw/";rootId=15;version=1};
                                        {id=6;parentId=Some(5);name="";rootPath="/Users/rw/";rootId=15;version=1};
                                        {id=7;parentId=Some(5);name="";rootPath="/Users/rw/";rootId=15;version=1};
                                {id=12;parentId=Some(1);name="";rootPath="/Users/rw/";rootId=15;version=1};
                                {id=9;parentId=Some(1);name="";rootPath="/Users/rw/";rootId=15;version=1};
                                    {id=20;parentId=Some(9);name="";rootPath="/Users/rw/";rootId=15;version=1};
                                {id=11;parentId=Some(1);name="";rootPath="/Users/rw/";rootId=15;version=1};
                                {id=14;parentId=Some(1);name="";rootPath="/Users/rw/";rootId=15;version=1};
                                    {id=19;parentId=Some(14);name="";rootPath="/Users/rw/";rootId=15;version=1};
                                    {id=16;parentId=Some(14);name="";rootPath="/Users/rw/";rootId=15;version=1};
                                    {id=15;parentId=Some(14);name="";rootPath="/Users/rw/";rootId=15;version=1};

                        ]
                        
                        sutResponse = None
                        currentFileId = 5
                        deletedFileVersion = 0
                        currentDirId = 22
                        currentUpdatedFile = 0
                        currentUpdatedDirId = 0
                        rights = [{dir = "17";rights = R; user= "101"}; {dir = "18";rights = R; user = "101"}; {dir = "9"; rights= R; user = "101"}; {dir = "16"; rights=CRUD;user="101"}; {dir = "20";rights=R;user="101"}
                                  {dir = "17";rights = CRUD; user= "100"}; {dir = "18";rights = CRUD; user = "100"}; {dir = "9"; rights= CRUD; user = "100"}; {dir = "15"; rights=CRUD;user="100"};{dir = "20";rights=CRUD;user="100"}]
                    
                    }
                    model
            
            }
        
        
        
        //let fileNameGen = Gen.elements ["test1.txt"; "test2.txt" ]
        //let dirIdGen = Gen.choose(15,15)
        //let fileIdGen = Gen.choose(5,5)
        
        //let fileVersionGen = Gen.choose(1,1)
        { new Machine<apiModel,Model>() with
            member __.Setup = create |> Gen.constant |> Arb.fromGen
                           
            member __.Next model =
                let userIdGen = Gen.frequency [(2, Gen.elements ["100"]); (2, Gen.elements ["101"]);(1, Gen.elements ["1"])]
                let fileVersionGen = createFileVersionGen model
                let dirIdGen = createDirectoryGen model.directories
                let fileIdGen = createFileIdGenerator model
                let ContentGenerator =  Gen.elements ["This is a new file"; "content A" ]
                let timestampGen = Gen.elements ["637479675580000000"]
                let fileNameGen = Gen.elements ["test1.txt"; "filenameA"; "filenameB" ]
                
                
                
                let uploadFileGen = [  Gen.map5  uploadFile ContentGenerator userIdGen fileIdGen fileVersionGen timestampGen ]
                let downloadFileGen = [Gen.map2 downloadFile userIdGen fileIdGen]
                let listFilesGen = [Gen.map listFiles userIdGen ]
                let fileMetaInformationGen = [Gen.map2 fileMetaInformation userIdGen fileIdGen ]
                let dirStrcutureGen = [Gen.map dirStructure userIdGen]
                let createFileGen = [Gen.map4 createFile userIdGen dirIdGen fileNameGen timestampGen ] 
                let moveFileGen = [Gen.map5 movefile userIdGen fileIdGen dirIdGen fileNameGen fileVersionGen ]
                let updateTimestampGen = [Gen.map3 updateFileTimeStamp userIdGen fileIdGen fileVersionGen ]
                let fileDeleteGen = [Gen.map3 fileDelete userIdGen fileIdGen fileVersionGen ]
                let DirCreateGen = [Gen.map4 createDirectory userIdGen dirIdGen fileNameGen fileVersionGen ]
                
                Gen.oneof ( fileMetaInformationGen @uploadFileGen@ downloadFileGen@ fileMetaInformationGen @updateTimestampGen  @ DirCreateGen @  fileDeleteGen @ createFileGen  @ listFilesGen   @ dirStrcutureGen @moveFileGen  )
                }
                

    let config =  {Config.Verbose with Replay = Some <| Random.StdGen (980257697,296888203); MaxTest = 100  }
   // let config = {Config.Verbose with MaxTest = 70  }
    type stateTest =
        static member ``test2`` = StateMachine.toProperty orbitAPI 
   // Check.One(config ,(StateMachine.toProperty spec)  )
    
    
    let start = Check.One(config ,(StateMachine.toProperty orbitAPI))
    

    

    
        
