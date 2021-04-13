namespace OrbitTestSuite.TestSuite

open FsCheck
open FsCheck.Experimental


open OrbitTestSuite.Models
open OrbitTestSuite.API
open OrbitTestSuite.Utilities
open OrbitTestSuite.ConfigureModel
open OrbitTestSuite.InMemoryModel

module testSuite =


    let generateStrings = Gen.constant "100" |> Gen.sample 0 10 |> List.head 
    
    let startTestSuite = 
        
        let inMemoryModel = ConfigureModel.intializeInMemoryModel

        let getFiles  = { new Command<inMemoryModels.inMemoryModel,inMemoryModels.inMemoryModel>() with
                    override __.RunActual apiModel = apiModel.GetFilesAPI (string apiModel.GetUser.id); apiModel
                    override __.RunModel inMemoryModel = inMemoryModel.GetFiles; inMemoryModel
                    override __.Post(apiModel, inMemoryModel) = Utilities.compareFilesMetadata apiModel.GetFilesResponse.data.fileList inMemoryModel.GetFiles = true |@ sprintf "model: %A <> %A" apiModel.GetFilesResponse.data.fileList inMemoryModel.GetFiles
                    override __.ToString() = "GetFiles" }                          
        { new ICommandGenerator<inMemoryModels.inMemoryModel,inMemoryModels.inMemoryModel> with
            member __.InitialActual = inMemoryModel
            member __.InitialModel = inMemoryModel
            member __.Next model = Gen.elements [ getFiles;] }

    let config =  {Config.Quick with MaxTest = 1}
    type stateTest =
        static member ``test2`` = Command.toProperty startTestSuite 

    let setupTestSuite = Check.All<stateTest>(config)
    
    
        
    

    
        