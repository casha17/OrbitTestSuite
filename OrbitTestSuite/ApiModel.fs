namespace OrbitTestSuite.InMemoryModel
open System.Collections.Generic
open OrbitTestSuite.API
open OrbitTestSuite.DockerIntegration
open OrbitTestSuite.Models.ApiResponseModels
open OrbitTestSuite.Models.Model

open HttpFs.Client
    
    type DockerEngine = {
        mutable stopString : string
        startString : string
        address : string
    }
    
    [<StructuredFormatDisplay("test")>]
    type apiModel() =
    
        let mutable fileVersion = 0;
        
 
        member __.GetCurrentfileVersion = fileVersion
        member __.PostFile content userId id fileId version timestamp =
          content
           
        member __.ToString = "ss"
         
