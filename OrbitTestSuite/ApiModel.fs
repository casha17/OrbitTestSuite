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
        let Engines = [{
            stopString = "docker stop orbit"
            startString = "docker run -d --name orbit --rm -p8085:8085 -eCLICOLOR_FORCE=2 cr.orbit.dev/sdu/filesync-server:latest"
            address = "http://localhost:8085/"
        };
        {
            stopString = "docker stop orbit1"
            startString = "docker run -d --name orbit1 --rm -p8084:8085 -eCLICOLOR_FORCE=2 cr.orbit.dev/sdu/filesync-server:latest"
            address = "http://localhost:8084/"
        };
          {
            stopString = "docker stop orbit2"
            startString = "docker run -d --name orbit2 --rm -p8083:8085 -eCLICOLOR_FORCE=2 cr.orbit.dev/sdu/filesync-server:latest"
            address = "http://localhost:8083/"
        };
          {
            stopString = "docker stop orbit3"
            startString = "docker run -d --name orbit3 --rm -p8082:8085 -eCLICOLOR_FORCE=2 cr.orbit.dev/sdu/filesync-server:latest"
            address = "http://localhost:8083/"
        };
          {
            stopString = "docker stop orbit4"
            startString = "docker run -d --name orbit4 --rm -p8081:8085 -eCLICOLOR_FORCE=2 cr.orbit.dev/sdu/filesync-server:latest"
            address = "http://localhost:8083/"
        };
          {
            stopString = "docker stop orbit5"
            startString = "docker run -d --name orbit5 --rm -p8080:8085 -eCLICOLOR_FORCE=2 cr.orbit.dev/sdu/filesync-server:latest"
            address = "http://localhost:8083/"
        };
        ]
        
        let mutable counter = 0
        member __.GetCurrent =
            Engines.[counter]
        member __.Next =
            let r = Docker.executeShellCommand Engines.[counter].stopString |> Async.RunSynchronously
            let r = Docker.executeShellCommand Engines.[counter].startString |> Async.RunSynchronously
            let r = Docker.executeShellCommand Engines.[counter].startString |> Async.RunSynchronously
            counter <- counter+1
            if (counter = 6)
            then
                counter <- 0
            
             


        
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
