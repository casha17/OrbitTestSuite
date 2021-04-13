namespace OrbitTestSuite.TestSuite

open FsCheck
open FsCheck.Experimental

open OrbitTestSuite.directoryget
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

    
    
   

    (*
    let spec =
        let inc = 
            { new Operation<inMemoryModels.inMemoryModel,int>() with
                member __.Run m = m + 1
                member __.Check (c,m) = 
                    let res = c.Inc() 
                    m = res 
                    |@ sprintf "Inc: model = %i, actual = %i" m res
                override __.ToString() = "inc"}
        let create initialValue = 
            { new Setup<Counter,int>() with
                member __.Actual() = new Counter(initialValue)
                member __.Model() = initialValue }
        { new Machine<Counter,int>() with
            member __.Setup = Gen.choose (0,3) |> Gen.map create |> Arb.fromGen
            member __.Next _ = Gen.elements [ inc; dec ] }
    *)
    
    let config =  {Config.Quick with MaxTest = 1}
    type stateTest =
       // static member ``test1`` = fun (e:List<RecordType>) -> List.isEmpty e  = List.isEmpty e
        static member ``test2`` = Command.toProperty startTestSuite 
    //let basicApiTests = Check.All 
    let setupTestSuite = Check.All<stateTest>(config)
    
    
        
    

    
        