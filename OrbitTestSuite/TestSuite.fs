namespace OrbitTestSuite.TestSuite

open FsCheck
open FsCheck.Experimental
open OrbitTestSuite.CounterTest
open OrbitTestSuite.HttpExample
open OrbitTestSuite.HttpExample.Http
open OrbitTestSuite.directoryget

module testSuite =
    
    let config =  {Config.Quick with MaxTest = 1}
    type stateTest =
        //static member ``test1`` = fun (e:List<RecordType>) -> List.isEmpty e  = List.isEmpty getContent
        static member ``test2`` = StateMachine.toProperty directoryget.testSuite
    //let basicApiTests = Check.All 
    let setupTestSuite = Check.VerboseAll<stateTest>()
    
    
        
    

    
        