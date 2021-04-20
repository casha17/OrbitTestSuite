namespace OrbitTestSuite.InMemoryModel
open OrbitTestSuite.API
open OrbitTestSuite.Models.ApiResponseModels
open OrbitTestSuite.Models.Model

open HttpFs.Client

    [<StructuredFormatDisplay("test")>]
    type apiModel(?init:int) =
          

       
        let mutable fileVersion =   0  
            
        member __.Initilize =
            let fileVersion = API.listFiles "100"
            let s = fileVersion.data.fileList |> Seq.map (fun e -> {users = [{files = [{content = ""; fileId= "2"; fileVersion = 1}]; userId = "2"}]} )
            s
      
        member __.GetCurrentfileVersion = fileVersion
        member __.PostFile content userId id fileId version timestamp =
           let s = (API.fileupload content userId id fileId version timestamp)
           fileVersion <- s.data.version
           s
           
         member __.ToString = "ss"
           
