namespace OrbitTestSuite.TestSuite


open FsCheck
open FsCheck.Experimental


open FsCheck.Random
open OrbitTestSuite.Models.Model
open OrbitTestSuite.Models.ApiResponseModels
open OrbitTestSuite.API
open OrbitTestSuite.Utilities
open OrbitTestSuite.InMemoryModel

module testSuite =
    let spec =
        let mutable currentFileId = 5
        let mutable currentDirId = 22
        let mutable currentUpdatedDir  = 0
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
            
        let downloadFile userId (fileId:int) = 
            { new Operation<apiModel,Model>() with
                member __.Run model = model
                member op.Check (sut,model) =
                    let modelResponse = Utilities.downloadFileModel model userId  fileId
                    let sutResponse = API.downloadFile userId (string fileId)
                    match sutResponse.Fail , sutResponse.Success , modelResponse.Fail , modelResponse.Success with
                        | None , Some c , None , Some m ->
                                let v = c = m
                                v.ToProperty
                        | Some sutc , None , Some model , None -> 
                            match sutc , model with
                                | Unauthorized x , Unauthorized y  -> true.ToProperty
                                | FileNotFound x , FileNotFound y -> true.ToProperty
                                | _  , _  -> false.ToProperty
                    |@ sprintf "DownloadFile: %i " fileId
                  
                override __.ToString() = sprintf "DownloadFile for user=%s fileid=%i" userId fileId}
     
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
        let createFile (userId:string) dirId fileName  = 
            { new Operation<apiModel,Model>() with
                member __.Run model =
                    let modelResponse = Utilities.createFileModel model userId dirId fileName
                    match modelResponse.Fail , modelResponse.Success with
                        | None , Some newModel  ->  currentFileId <- newModel.currentFileId; newModel
                        | Some error , Some newModel  -> currentFileId <- newModel.currentFileId; newModel
                        | _ , _ -> model
                member op.Check (sut,model) =
                    let sutResponse = API.createFile userId (string dirId) fileName "637479675580000000"
                    match sutResponse.Fail , sutResponse.Success  with
                        | _ , _t  ->  true.ToProperty
                    |@ sprintf "FileMetaInformation: "
                override __.ToString() = sprintf "Create file for user=%s dirId=%i filename=%s" userId dirId fileName 
            }
            
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
                    |@ sprintf "FileMetaInformation: "
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
        let create = 
            { new Setup<apiModel,Model>() with
                member __.Actual() = apiModel()
                member __.Model() =
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
                    let l = v |> List.map (fun e ->
                        {e with userFiles = e.userFiles.Add("16" , CRUD).Add("9" , R).Add("18", R).Add("17",R)}
                        )
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
                    
                    {users = g@l; files = files; currentFileId = currentFileId; deletedFileVersion = 0; currentUpdatedFile = 0; currentDirId = currentDirId; currentUpdatedDirId = 0}
                    
         }
            
            
        let name = Gen.elements ["a"; "b"; "c"; "d"]
        let user = Gen.elements ["100"; "101"]
        let fileNameGen = Gen.elements ["test1.txt"; "test2.txt";  ]
        let fileIdGen = Gen.choose(2,5)
        let dirIdGen = Gen.choose(14,16)
        { new Machine<apiModel,Model>() with
            member __.Setup = create |> Gen.constant |> Arb.fromGen
            member __.Next _ =
                let uploadFileGen = [  Gen.map3  uploadFile name user fileIdGen ]
                let downloadFileGen = [Gen.map2 downloadFile user fileIdGen]
                let listFilesGen = [Gen.map listFiles user ]
                let fileMetaInformationGen = [Gen.map2 fileMetaInformation user fileIdGen ]
                let dirStrcutureGen = [Gen.map dirStructure user]
                let createFileGen = [Gen.map3 createFile user dirIdGen fileNameGen ]
                let moveFileGen = [Gen.map4 movefile user fileIdGen dirIdGen fileNameGen ]
                let updateTimestampGen = [Gen.map2 updateFileTimeStamp user fileIdGen ]
                let fileDeleteGen = [Gen.map2 fileDelete user fileIdGen ]
                let createDirectoryGen = [Gen.map3 createDirectory user dirIdGen fileNameGen]
                Gen.oneof (fileDeleteGen @   downloadFileGen @uploadFileGen  @  dirStrcutureGen@  createFileGen @ moveFileGen  @ updateTimestampGen@listFilesGen @ fileMetaInformationGen  @ dirStrcutureGen) }

    let config =  {Config.Verbose with MaxTest = 400}
    
    type stateTest =
        static member ``test2`` = StateMachine.toProperty spec 

   // Check.One(config ,(StateMachine.toProperty spec)  )
    
    let start = Check.One(config ,(StateMachine.toProperty spec))
    
    
        
    

    
        