namespace OrbitTestSuite.InMemoryModel
open OrbitTestSuite.API
open OrbitTestSuite.Models.ApiResponseModels
open OrbitTestSuite.Models.Model

open HttpFs.Client

    [<StructuredFormatDisplay("test")>]
    type apiModel(?init:int) =
           
        let mutable fileVersion =   0  

      (*
        member __.GetCurrentfileVersion = fileVersion
        member __.PostFile content userId id fileId version timestamp =
           let s = (API.fileupload content userId id fileId version timestamp)
           match s.data with
                | None -> ()
                | Some c -> fileVersion <- c.version
           s
           
         member __.ToString = "ss"
           *)
