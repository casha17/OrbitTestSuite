namespace OrbitTestSuite.TestSuite

open FsCheck
open FsCheck.Experimental


open OrbitTestSuite.Models.Model
open OrbitTestSuite.Models
open OrbitTestSuite.API
open OrbitTestSuite.Utilities
open OrbitTestSuite.InMemoryModel

module testSuite =
    let spec =        
        let post content userId = 
            { new Operation<apiModel,Model>() with
                member __.Run model = Utilities.uploadFileModel model content userId
                member op.Check (sut,model) =
                    Utilities.uploadFileSut model content userId |@ sprintf "post content %s " content 
                override __.ToString() = sprintf "Post content: %s toUser:%s" content userId
            }
            
        let get userId = 
            { new Operation<apiModel,Model>() with
                member __.Run model = model
                member op.Check (sut,model) =
                    let modelResponse = Utilities.downloadFileModel model userId
                    let sutResponse = Utilities.downloadFileSut userId
                    (sutResponse.data = modelResponse.content).ToProperty
                    |@ sprintf "DownloadFile: sut=%s model=%s  " sutResponse.data modelResponse.content
                override __.ToString() = sprintf "DownloadFile for user=%s" userId}
       
        let create = 
            { new Setup<apiModel,Model>() with
                member __.Actual() = apiModel()
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
         
        let name = Gen.elements ["a"; "b"; "c"; "d"]
        let s = Gen.elements ["100"; "101"]
        { new Machine<apiModel,Model>() with
            member __.Setup = create |> Gen.constant |> Arb.fromGen
            member __.Next _ =
                let killOrReg = [  Gen.map2 post name s ]
                let s = [Gen.map get s]
                Gen.oneof (killOrReg @ s) }


    let config =  {Config.Verbose with MaxTest = 7;   }
    
    type stateTest =
        static member ``test2`` = StateMachine.toProperty spec 

   // Check.One(config ,(StateMachine.toProperty spec)  )
    
    let start = Check.One(config ,(StateMachine.toProperty spec))
    
    
        
    

    
        