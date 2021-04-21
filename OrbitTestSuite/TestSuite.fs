namespace OrbitTestSuite.TestSuite

open System.Collections.Generic
open FsCheck
open FsCheck.Experimental


open OrbitTestSuite.Models.Model
open OrbitTestSuite.Models.ApiResponseModels
open OrbitTestSuite.API
open OrbitTestSuite.Utilities
open OrbitTestSuite.InMemoryModel

module testSuite =
    let spec =        
        let uploadFile content userId (fileId:int) = 
            { new Operation<apiModel,Model>() with
                member __.Run model =
                    let s = Utilities.uploadFileModel model content userId (string fileId)
                    match s with
                        | None ->  model
                        | Some s -> s

                member op.Check (sut,model) =
                    let s = Utilities.uploadFileSut model content userId (string fileId) 
                    match s with
                        | None ->  true.ToProperty
                        | Some c -> true.ToProperty
                    |@ sprintf "post content %s " content
                override __.ToString() = sprintf "uploadFile: content: %s toUser:%s fileId:%i" content userId fileId
            }
            
        let downloadFile userId (fileId:int) = 
            { new Operation<apiModel,Model>() with
                member __.Run model = model
                member op.Check (sut,model) =
                    let modelResponse = Utilities.downloadFileModel model userId (string fileId)
                    let sutResponse = Utilities.downloadFileSut userId (string fileId)
                    match sutResponse , modelResponse with
                        | Some sut , Some model -> (sut.data = model.content).ToProperty
                        | None , None -> true.ToProperty
                        | _ , _ -> false.ToProperty
                    |@ sprintf "DownloadFile: "
                override __.ToString() = sprintf "DownloadFile for user=%s fileid=%i" userId fileId}
        
        let listFiles userId = 
            { new Operation<apiModel,Model>() with
                member __.Run model = model
                member op.Check (sut,model) =
                    let modelResponse = Utilities.listFilesModel model userId
                    let sutResponse = Utilities.listFilesSut userId
                    match sutResponse.Fail , sutResponse.Success , modelResponse.Fail , modelResponse.Success with
                        | None , Some sut , None , Some model ->  (model.fileList = sut.fileList).ToProperty
                        | Some sut , None , Some model , None -> 
                            match sut , model with
                                | NoUserIdSupplied x , NoUserIdSupplied y  -> true.ToProperty
                                | _  , _  -> false.ToProperty
                        | _ , _ , _ , _ -> printf "should never reach this state"; false.ToProperty
                    |@ sprintf "list files: "
                override __.ToString() = sprintf "DownloadFile for user=%s" userId
            }
            
        let fileMetaInformation userId (fileId:int) = 
            { new Operation<apiModel,Model>() with
                member __.Run model = model
                member op.Check (sut,model) =
                    let modelResponse = Utilities.fileMetaInformationModel model userId  fileId
                    let sutResponse = API.fileMetaInformationByFileId userId (string fileId)
                    match sutResponse.Fail , sutResponse.Success , modelResponse.Fail , modelResponse.Success with
                        | None , Some sut , None , Some model ->  (model = sut).ToProperty
                        | Some sut , None , Some model , None -> 
                            match sut , model with
                                | FileNotFound x , FileNotFound y  -> true.ToProperty
                                | _  , _  -> false.ToProperty
                        | _ , _ , _ , _ -> printf "should never reach this state"; false.ToProperty
                    |@ sprintf "FileMetaInformation: "
                override __.ToString() = sprintf "fileMetaInformation for user=%s fileId=%i" userId fileId
            }
       
        let create = 
            { new Setup<apiModel,Model>() with
                member __.Actual() = apiModel()
                member __.Model() =
                    let listFilesResult = API.listFiles "100"
                    let temp =
                        Some(listFilesResult.Success.Value.fileList).Value |> List.map (fun e ->
                            let s = API.downloadFile "100" (string e.id)
                            {
                            files = [{fileVersion = e.version; content = match s with
                                  | Some c -> c.data
                                  ; fileId = (string e.id)};]
                            fileList = [{id =  e.id; name = e.name; parentId =  e.parentId; version = e.version; versionChanged = e.versionChanged; timestamp = e.timestamp}]
                            directoryVersions = []
                            userId = "100"
                        }
                        )
                    let dir = Some(listFilesResult.Success.Value.directoryVersions).Value |> List.map (fun e -> {
                        id =  e.id
                        version =  e.version
                    } )
                    let s = temp |> List.map (fun  e -> {e with directoryVersions = dir})
                    let listFilesResult = API.listFiles "101"
                    let temp =
                        Some(listFilesResult.Success.Value.fileList).Value |> List.map (fun e ->
                        let s = API.downloadFile "101" (string e.id)
                        {
                        files = [{fileVersion = e.version; content = match s with
                                  | Some c -> c.data
                                  ; fileId = (string e.id)};]
                        fileList = [{id =  e.id; name = e.name; parentId =  e.parentId; version = e.version; versionChanged = e.versionChanged; timestamp = e.timestamp}]
                        directoryVersions = []
                        userId = "101"
                        }
                        )
                    let dir = Some(listFilesResult.Success.Value.directoryVersions).Value |> List.map (fun e -> {
                        id =  e.id
                        version =  e.version
                    } )
                    let v = temp |> List.map (fun  e -> {e with directoryVersions = dir})
                    {users = s@v}
                    }
         
        let name = Gen.elements ["a"; "b"; "c"; "d"]
        let user = Gen.elements ["100"; "101"]
        let fileIdGen = Gen.choose(2,3)
        { new Machine<apiModel,Model>() with
            member __.Setup = create |> Gen.constant |> Arb.fromGen
            member __.Next _ =
                let uploadFileGen = [  Gen.map3  uploadFile name user fileIdGen ]
                let downloadFileGen = [Gen.map2 downloadFile user fileIdGen]
                let listFilesGen = [Gen.map listFiles user ]
                let fileMetaInformationGen = [Gen.map2 fileMetaInformation user fileIdGen ]
                Gen.oneof (uploadFileGen @ downloadFileGen @ listFilesGen @ fileMetaInformationGen  ) }


    let config =  {Config.Quick with MaxTest = 100;   }
    
    type stateTest =
        static member ``test2`` = StateMachine.toProperty spec 

   // Check.One(config ,(StateMachine.toProperty spec)  )
    
    let start = Check.One(config ,(StateMachine.toProperty spec))
    
    
        
    

    
        