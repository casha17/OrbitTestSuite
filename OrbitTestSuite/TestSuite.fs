namespace OrbitTestSuite.TestSuite


open System.Threading
open FsCheck
open FsCheck.Arb
open FsCheck.Experimental
open OrbitTestSuite.DockerIntegration
open OrbitTestSuite.Models.Model
open OrbitTestSuite.Models.ApiResponseModels
open OrbitTestSuite.API
open OrbitTestSuite.Utilities
open OrbitTestSuite.InMemoryModel

module testSuite =
    let spec =

        
        (*
        let uploadFile content userId (fileId:int) = 
            { new Operation<apiModel,Model>() with
                member __.Run model =
                    let s = Utilities.uploadFileModel model content userId  fileId
                    match s.Fail , s.Success with
                        | None , Some c ->  c
                        | Some c , None-> model
                        | _ , _ -> model

                member op.Check (sut,model) =
                    let response = Utilities.uploadFileSut model content userId fileId 
                    match response.Fail , response.Success with
                        | _ , _  ->  true.ToProperty // True in both writing and not writing
                    |@ sprintf "post content %s " content
                override __.ToString() = sprintf "uploadFile: content: %s toUser:%s fileId:%i" content userId fileId
            }
            *)
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
     (*
        let listFiles userId = 
            { new Operation<apiModel,Model>() with
                member __.Run model = model
                member op.Check (sut,model) =
                    let modelResponse = Utilities.listFilesModel model userId
                    let sutResponse = API.listFiles userId
                    match sutResponse.Fail , sutResponse.Success , modelResponse.Fail , modelResponse.Success with
                        | None , Some s , None , Some m ->
                        let g = s.fileList |> List.sortBy (fun e -> e.id)
                        let x = m.fileList |> List.sortBy (fun e -> e)
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
                member __.Run model = model
                member op.Check (sut,model) =
                    let modelResponse = Utilities.fileMetaInformationModel model userId   fileId
                    let sutResponse = API.fileMetaInformationByFileId userId (string fileId)
                    match sutResponse.Fail , sutResponse.Success , modelResponse.Fail , modelResponse.Success with
                        | None , Some s , None , Some m ->
                            let v = s = m
                            v.ToProperty()
                        | Some sut , None , Some model , None -> 
                            match sut , model with
                                | FileNotFound x , FileNotFound y  -> true.ToProperty()
                                | Unauthorized x , Unauthorized y -> true.ToProperty()
                                | _  , _  -> false.ToProperty()
                        | _ , _ , _ , _ -> printf "should never reach this state"; false.ToProperty()
                    |@ sprintf "model:  %A SUT: %A" modelResponse.Success  sutResponse.Success
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
                        let v = m = s
                        v.ToProperty
                        | Some sut , None , Some model , None -> 
                            match sut , model with
                                | FileNotFound x , FileNotFound y  -> true.ToProperty
                                | Unauthorized x , Unauthorized y -> true.ToProperty
                                | _  , _  -> false.ToProperty
                        | _ , _ , _ , _ -> printf "should never reach this state"; false.ToProperty
                    |@ sprintf "FileMetaInformation: "
                override __.ToString() = sprintf "DirStructure for user=%s " userId 
            }
            *)
        let createFile (userId:string) dirId fileName  = 
            { new Operation<apiModel,Model>() with
                member __.Run model =
                    let modelResponse = Utilities.createFileModel model userId dirId fileName
                    match modelResponse.Fail , modelResponse.Success with
                        | None , Some newModel  ->   newModel
                        | Some error , Some newModel -> {newModel with sutResponse = Some(error) }
                member op.Check (sut,model) =
                    let sutResponse = API.createFile userId (string dirId) fileName "637479675580000000"
                    match sutResponse.Fail,sutResponse.Success , model.sutResponse with
                        | None , Some sut , Some model  -> match model with
                            | CreateFileSuccess m ->
                                let s = (sut = m)
                                s.ToProperty
                            | _ -> false.ToProperty
                        | Some sutError , None , Some modelError -> match sutError , modelError with
                            | Unauthorized , Unauthorized -> true.ToProperty
                            | Conflict , Conflict -> true.ToProperty
                            | NotFound , NotFound -> true.ToProperty
                            | _ , _ -> false.ToProperty
                        | _ , _ , _ -> false.ToProperty
                                
                    |@
                     match sutResponse.Fail,sutResponse.Success , model.sutResponse with
                        | None , Some sut , Some model  -> match model with
                            | CreateFileSuccess m -> sprintf "Success: SUT:%A Model:%A" sut m 
                            | _ -> sprintf "createfile: SUT:%A Model:%A" sut model 
                        | Some sutError , None , Some modelError -> match sutError , modelError with
                            | Unauthorized , Unauthorized ->sprintf "createfile Aunathorized : SUT: Model:" 
                            | Conflict , Conflict -> sprintf "createfile: Conflict SUT: Model:" 
                            | NotFound , NotFound -> sprintf "createfile: NotFound SUT: Model:"  
                            | _ , _ -> sprintf "createfile: SUT:%A Model:%A" sutError modelError 
                        | _ , _ , _ -> sprintf "createfile: SUT:%A Model:%A - " sut model 
                override __.ToString() = sprintf "Create file for user=%s dirId=%i filename=%s" userId dirId fileName 
            }
            (*
        let movefile (userId:string) fileId dirId newFileName  = 
            { new Operation<apiModel,Model>() with
                member __.Run model =
                    let modelResponse = Utilities.moveFileModel model userId fileId dirId newFileName
                    match modelResponse.Fail , modelResponse.Success with
                        | None , Some newModel  ->  currentFileId <- newModel.currentFileId; newModel
                        | Some error , Some newModel  -> currentFileId <- newModel.currentFileId; newModel
                        | _ , _ -> model
                member op.Check (sut,model) =
                    let sutResponse = Utilities.MoveFileSut model userId fileId dirId newFileName 
                    match sutResponse.Fail , sutResponse.Success  with
                        | _ , _t  ->  true.ToProperty
                    |@ sprintf "moveFile: "
                override __.ToString() = sprintf "Move file for user=%s dirId=%i fileId=%i filename=%s" userId dirId fileId  newFileName
            }    
                
         
        let updateFileTimeStamp (userId:string) fileId   = 
            { new Operation<apiModel,Model>() with
                member __.Run model =
                    let modelResponse = Utilities.updateFileTimestampModel model userId fileId "637479675580000000" 
                    match modelResponse.Fail , modelResponse.Success with
                        | None , Some newModel  -> newModel
                        | Some error , Some newModel  -> model
                        | _ , _ -> model
                member op.Check (sut,model) =
                    let sutResponse = Utilities.updateFileTimestampSut model userId fileId "637479675580000000"  
                    match sutResponse.Fail , sutResponse.Success  with
                        | _ , _t  ->  true.ToProperty
                    |@ sprintf "UpdateTimestamp: "
                override __.ToString() = sprintf "update timestamp file for user=%s fileId=%i  " userId  fileId  
            }
            
        let fileDelete (userId:string) fileId   = 
            { new Operation<apiModel,Model>() with
                member __.Run model =
                    let modelResponse = Utilities.fileDeleteModel model userId fileId 
                    match modelResponse.Fail , modelResponse.Success with
                        | None , Some newModel  -> newModel
                        | Some error , Some newModel  -> model
                        | _ , _ -> model
                member op.Check (sut,model) =
                    let sutResponse = Utilities.deleteFileSut model userId fileId   
                    match sutResponse.Fail , sutResponse.Success  with
                        | _ , _t  ->  true.ToProperty
                    |@ sprintf "Delete File: fileID "
                override __.ToString() = sprintf "Delete file for user=%s fileId=%i  " userId  fileId  
            }
            
        let createDirectory (userId:string) dirId fileName  = 
            { new Operation<apiModel,Model>() with
                member __.Run model =
                    let modelResponse = Utilities.createDirectoryModel model userId dirId fileName
                    match modelResponse.Fail , modelResponse.Success with
                        | None , Some newModel  ->  currentFileId <- newModel.currentFileId; newModel
                        | Some error , Some newModel  -> currentFileId <- newModel.currentFileId; newModel
                        | _ , _ -> model
                member op.Check (sut,model) =
                    let sutResponse = API.directoryCreate userId (string dirId) fileName "1"
                    match sutResponse.Fail , sutResponse.Success  with
                        | _ , _t  ->  true.ToProperty
                    |@ sprintf "FileMetaInformation:"
                override __.ToString() = sprintf "Create file for user=%s dirId=%i filename=%s" userId dirId fileName 
            }
            
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
                member __.Actual() = apiModel()
                member __.Model() =
                    
                    let r = Docker.executeShellCommand "docker stop orbit" |> Async.RunSynchronously
                    let r = Docker.executeShellCommand "docker run -d --name orbit --rm -p8085:8085 -eCLICOLOR_FORCE=2 cr.orbit.dev/sdu/filesync-server:latest" |> Async.RunSynchronously
                    Thread.Sleep 5000
                    {
                        users = [
                            {
                                userId = "100"
                                dirStructures = [
                                    {id=15;parentId=None;name="";rootPath="/Users/rw/";rootId=15;rootIsDefault=true;version=1}
                                    {id=17;parentId=None;name="";rootPath="/Projects/Project 1/";rootId=17;rootIsDefault=false;version=1}
                                    {id=18;parentId=None;name="";rootPath="/Projects/Project 2/";rootId=18;rootIsDefault=false;version=1}
                                ]
                            };
                              {
                                
                                userId = "101"
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
                        //directoryVersions = [{id=15; version = 1};{id=17;version=1};{id=18;version=1}]
                        sutResponse = None
                        currentFileId = 5
                        deletedFileVersion = 0
                        currentDirId = 0
                        currentUpdatedFile = 0
                        currentUpdatedDirId = 0
                        rights = [{dir = "17";rights = R; user= "101"}; {dir = "18";rights = R; user = "101"}; {dir = "9"; rights= R; user = "101"}; {dir = "16"; rights=R;user="101"}; {dir = "20";rights=R;user="101"}
                                  {dir = "17";rights = CRUD; user= "100"}; {dir = "18";rights = CRUD; user = "100"}; {dir = "9"; rights= CRUD; user = "100"}; {dir = "15"; rights=CRUD;user="100"};{dir = "20";rights=CRUD;user="100"}]
                    }

            
            }
        let name = Gen.elements ["a"; "b"; "c"; "d"]
        let user = Gen.elements ["100"]
        let fileNameGen = Gen.elements ["test1.txt";]
        let dirIdGen = Gen.choose(17,17)
        let fileIdGen = Gen.choose
        { new Machine<apiModel,Model>() with
            member __.Setup = create |> Gen.constant |> Arb.fromGen
            member __.Next model =
                let fileIds = Utilities.getAllFileIds model.files
                let dirIds = Utilities.getALlDirIds model.directories
                let fileIdGen = Gen.frequency [(5 ,Gen.elements fileIds); (1 ,Gen.choose(40,41))]
                let dirIdGen = Gen.frequency [(5 ,Gen.elements dirIds); (1 ,Gen.choose(1,50))]
             //   let uploadFileGen = [  Gen.map3  uploadFile name user fileIdGen ]
                let downloadFileGen = [Gen.map2 downloadFile user fileIdGen]
             //   let listFilesGen = [Gen.map listFiles user ]
             //   let fileMetaInformationGen = [Gen.map2 fileMetaInformation user fileIdGen ]
             //   let dirStrcutureGen = [Gen.map dirStructure user]
                let createFileGen = [Gen.map3 createFile user dirIdGen fileNameGen ]
             //   let moveFileGen = [Gen.map4 movefile user fileIdGen dirIdGen fileNameGen ]
             //   let updateTimestampGen = [Gen.map2 updateFileTimeStamp user fileIdGen ]
             //   let fileDeleteGen = [Gen.map2 fileDelete user fileIdGen ]
             //   let createDirectoryGen = [Gen.map3 createDirectory user dirIdGen fileNameGen]
                Gen.oneof (createFileGen@downloadFileGen) }

    let config =  {Config.Verbose with Replay = Some <| Random.StdGen (13,1) }
    
    type stateTest =
        static member ``test2`` = StateMachine.toProperty spec 

   // Check.One(config ,(StateMachine.toProperty spec)  )
    
    let start = Check.One(config ,(StateMachine.toProperty spec))
    
    
        
    

    
        