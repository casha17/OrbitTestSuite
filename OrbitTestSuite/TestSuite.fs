namespace OrbitTestSuite.TestSuite

open FsCheck
open OrbitTestSuite.CounterTest
open OrbitTestSuite.HttpExample
open OrbitTestSuite.HttpExample.Http

module testSuite =
    
    let config =  {Config.Quick with MaxTest = 10}
    type tests =
        //static member ``test1`` = fun (e:List<RecordType>) -> List.isEmpty e  = List.isEmpty getContent
        static member ``test2`` = Command.toProperty InitialTest.spec
    let setupTestSuite = Check.All<tests>(config)
    
    
        
    

    
        