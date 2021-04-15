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
    
    // Make an int generater --> Sample a number with a maxSize of 100 and sample a single element
    let generateSeeds = Arb.generate<int> |> Gen.sample 100 1 |> List.head
    
    let startTestSuite = 
        
        let inMemoryModel = ConfigureModel.intializeInMemoryModel

        let getFiles = { new Command<inMemoryModels.inMemoryModel,inMemoryModels.inMemoryModel>() with
                    override __.RunActual apiModel = apiModel.GetFilesAPI (string apiModel.GetUser.id); apiModel
                    override __.RunModel inMemoryModel = inMemoryModel // There is no purpose of running an equivalent get locally
                    override __.Post(apiModel, inMemoryModel) = Utilities.compareFilesMetadata apiModel.GetFilesResponse.data.fileList inMemoryModel.GetFiles = true |@ sprintf "model: %A <> %A" apiModel.GetFilesResponse.data.fileList inMemoryModel.GetFiles
                    override __.ToString() = "GetFiles" }
        
        // Pick a random in memory file request its metadata and compare that the inmemory metadata representation is similar to the returned API call
        let getFileMetaDataByFileId seed = {new Command<inMemoryModels.inMemoryModel, inMemoryModels.inMemoryModel> () with
                    override __.RunActual apiModel = apiModel.GetFilesMetadataByFileId (string apiModel.GetUser.id) (string (apiModel.GetRandomFileId seed)); apiModel
                    override __.RunModel inMemoryModel = inMemoryModel // There is no purpose of running an equivalent get locally
                    override __.Post(apiModel, inMemoryModel) = Utilities.compareFileMetadata
                                                                    apiModel.GetFilesMetadataByFiledIdResponse.data
                                                                    (inMemoryModel.GetInMemoryFilesMetadataByFiledId (apiModel.GetRandomFileId seed)).[0].metadata = true
                                                                |@ sprintf "model: %A <> %A" apiModel.GetFilesResponse.data.fileList inMemoryModel.GetFiles
                    override __.ToString() = "GetFileMetaData"
                    }
        
        { new ICommandGenerator<inMemoryModels.inMemoryModel,inMemoryModels.inMemoryModel> with
            member __.InitialActual = inMemoryModel
            member __.InitialModel = inMemoryModel
            member __.Next model = Gen.elements [ getFiles; ]}//generateSeeds |> getFileMetaDataByFileId; ] }

    // The getFileMetaDataByFileId
    
    let config =  {Config.Quick with MaxTest = 1}
    type stateTest =
        static member ``test2`` = Command.toProperty startTestSuite 

    let setupTestSuite = Check.All<stateTest>(config)
    
    
        
    

    
        