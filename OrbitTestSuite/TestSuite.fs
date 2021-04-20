namespace OrbitTestSuite.TestSuite

open FsCheck
open FsCheck.Experimental


open OrbitTestSuite.InMemoryModel.inMemoryModels
open OrbitTestSuite.Models
open OrbitTestSuite.API
open OrbitTestSuite.Utilities
open OrbitTestSuite.ConfigureModel
open OrbitTestSuite.InMemoryModel

module testSuite =

    let ss = Gen.elements ["foo"; "bar"; "baz"] |> Gen.nonEmptyListOf |> Gen.sample 20 4 |> List.head |> List.head
    let generateStrings =  Gen.elements ["a"; "b"; "c"; "d"] 
    
    // Make an int generater --> Sample a number with a maxSize of 100 and sample a single element
    let generateSeeds = Arb.generate<int> |> Gen.sample 100 1 |> List.head
    
    //let startTestSuite = 
          (*
        let inMemoryModel = ConfigureModel.intializeInMemoryModel
      //  printf "%A" inMemoryModel.ToString

        let getFiles = { new Command<inMemoryModels.inMemoryModel,inMemoryModels.inMemoryModel>() with
                    override __.RunActual apiModel =printf "%s" "Actual"; apiModel.GetFilesAPI (string apiModel.GetUser.id); apiModel
                    override __.RunModel inMemoryModel =printf "%s" "inMemory"; inMemoryModel // There is no purpose of running an equivalent get locally
                    override __.Post(apiModel, inMemoryModel) =printf "%s" "POST"; Utilities.compareFilesMetadata apiModel.GetFilesResponse.data.fileList inMemoryModel.GetFiles = true |@ sprintf "model: %A <> %A" apiModel.GetFilesResponse.data.fileList inMemoryModel.GetFiles
                    override __.ToString() = "GetFiles" }
               
        // Pick a random in memory file request its metadata and compare that the inmemory metadata representation is similar to the returned API call
        let getFileMetaDataByFileId seed = {new Command<inMemoryModels.inMemoryModel, inMemoryModels.inMemoryModel> () with
                    override __.RunActual apiModel = apiModel.GetFilesMetadataByFileId (string apiModel.GetUser.id) (string (apiModel.GetRandomFileId seed)); apiModel
                    override __.RunModel inMemoryModel = inMemoryModel // There is no purpose of running an equivalent get locally
                    override __.Post(apiModel, inMemoryModel) = Utilities.compareFileMetadata
                                                                    apiModel.GetFilesMetadataByFiledIdResponse.data
                                                                    (inMemoryModel.GetInMemoryFilesMetadataByFiledId (apiModel.GetRandomFileId seed)) = true
                                                                |@ sprintf "model: %A <> %A" apiModel.GetFilesResponse.data.fileList inMemoryModel.GetFiles
                    override __.ToString() = "GetFileMetaData"
                    }
            
 
        let writeToFile content = { new Command<inMemoryModels.inMemoryModel,inMemoryModels.inMemoryModel>() with
                    override __.RunActual apiModel =printf "%s" "Actual"; API.fileupload content (string apiModel.GetUser.id) "2" (string (apiModel.getCurrentFileVersion 2)) (string (apiModel.getCurrentFileVersion 2))  "637479675580000000" |> ignore ; apiModel
                    override __.RunModel inMemoryModel = printf "%s" "inMemory"; inMemoryModel.WriteToLocalFile 2 content; inMemoryModel
                    override __.Post(apiModel, inMemoryModel) = Utilities.compareContent inMemoryModel.GetFiles 2 (API.downloadFile "100" "2").data = true  |@ sprintf "model: %A <> %A" apiModel.GetFilesResponse.data.fileList inMemoryModel.GetFiles
                    override __.ToString() = "WriteToFile" }


        { new ICommandGenerator<inMemoryModels.inMemoryModel,inMemoryModels.inMemoryModel> with
            member __.InitialActual = inMemoryModel
            member __.InitialModel = inMemoryModel
            member __.Next model = Gen.elements [getFiles; (*  generateSeeds |> getFileMetaDataByFileId; generateStrings |> writeToFile *)]}//generateSeeds |> getFileMetaDataByFileId; ] }
 *)
    // The getFileMetaDataByFileId
    
  
    let spec =        
        let post content  = 
            { new Operation<apiModel,inMemory>() with
                member __.Run apiModel =
                    let s = apiModel.users |> List.map (fun user ->
                        if user.userId = "100" then { user with files = user.files |> List.map (fun e -> {e with content = content; fileVersion = e.fileVersion+1 })} else user )
                    {apiModel with users = s}
                member op.Check (inMemoryModel,apiModel) =
                    let user = apiModel.users |> List.find (fun e-> e.userId = "100")
                    let file = user.files |> List.find (fun e -> e.fileId = "2")
                    let currentFileversion = file.fileVersion-1
                    let s = inMemoryModel.PostFile content "100" "2"  (string currentFileversion) (string currentFileversion) "637479675580000000" 
                    true.ToProperty()
                         |@ sprintf "post content %s " content 
                override __.ToString() =
                    sprintf "Post %s  " content 
                    }
            
        let get = 
            { new Operation<apiModel,inMemory>() with
                member __.Run apiModel = apiModel
                member op.Check (inMemoryModel,apiModel) =
                    let Apires = (API.downloadFile "100" "2")
                    //if res.data <> null then printf "%s" "NOT NULL" else printf "%s" "NULL"
                    let localRes = apiModel.users |> List.find (fun e -> e.userId = "100")
                    let s = localRes.files |> List.find (fun e-> e.fileId =  "2")
                    (Apires.data = s.content).ToProperty
                    //true.ToProperty
                        |@ sprintf "get API: %s Local:  " Apires.data   
                override __.ToString() = "GET"}
       
        let create = 
            { new Setup<apiModel,inMemory>() with
                member __.Actual() = new apiModel()
                member __.Model() =
                    let listFilesResult = API.listFiles "100"
                    let s =
                        listFilesResult.data.fileList |> List.map (fun e ->
                        let s = API.downloadFile "100" (string e.id)
                        {
                        files = [{fileVersion = e.version; content = s.data; fileId = (string e.id)};]
                        userId = "100"
                        }
                        )
                    let listFilesResult = API.listFiles "101"
                    let v =
                        listFilesResult.data.fileList |> List.map (fun e ->
                        let s = API.downloadFile "101" (string e.id)
                        {
                        files = [{fileVersion = e.version; content = s.data; fileId = (string e.id)};]
                        userId = "101"
                        }
                        )
                    {users = s@v}
                    }
         
         
        { new Machine<apiModel,inMemory>() with
            member __.Setup = create |> Gen.constant |> Arb.fromGen
            member __.Next _ =
                   let ss = Gen.elements ["foo"; "bar"; "baz"] |> Gen.nonEmptyListOf |> Gen.sample 20 4 |> List.head |> List.head
                   let name = Gen.elements ["a"; "b"; "c"; "d"]
                   Gen.elements [ ss  |>  post ;  get]
                   }


    let config =  {Config.Verbose with MaxTest = 7;   }
    
    type stateTest =
        static member ``test2`` = StateMachine.toProperty spec 

   // Check.One(config ,(StateMachine.toProperty spec)  )
    
    let setupTestSuite = Check.One(config ,(StateMachine.toProperty spec))
    
    
        
    

    
        