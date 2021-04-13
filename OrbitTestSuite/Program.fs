// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open OrbitTestSuite.TestSuite
// Define a function to construct a message to print



[<EntryPoint>]
let main argv =    
    
    testSuite.setupTestSuite
    0 // return an integer exit code