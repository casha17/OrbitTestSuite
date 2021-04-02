// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp


open FSharp.Json
open OrbitTestSuite.HttpExample
open OrbitTestSuite.TestSuite
open OrbitTestSuite.directoryget

// Define a function to construct a message to print



[<EntryPoint>]
let main argv =

    testSuite.setupTestSuite
    //let check e =  List.isEmpty e  = List.isEmpty Http.getContent
    

    //let rec check s  = match s with
    //| [] -> printf "empty"
    //| el::els -> check els; printf "\n\n  %A " el
    
    
    
    
    //check directoryget.bodyContent
   // Check.Verbose check
    0 // return an integer exit code