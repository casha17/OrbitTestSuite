// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp


open FSharp.Json
open HttpFs.Client
open Hopac
open OrbitTestSuite.Utilities
open OrbitTestSuite.TestSuite
open OrbitTestSuite.directoryget
open OrbitTestSuite.API
open OrbitTestSuite.ConfigureModel
open FsCheck
// Define a function to construct a message to print



[<EntryPoint>]
let main argv =    
    
    testSuite.setupTestSuite
    0 // return an integer exit code