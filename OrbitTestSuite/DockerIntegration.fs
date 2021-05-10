namespace OrbitTestSuite.DockerIntegration
open System
open System.Diagnostics
open System.Threading.Tasks
open Fake.Core

module Docker =
    
    type DockerEngine = {
        mutable stopString : string
        startString : string
        address : string
    }
    
    type CommandResult = { 
      ExitCode: int; 
      StandardOutput: string;
      StandardError: string 
    }
    
      let executeCommand executable args =
      async {
        let startInfo = ProcessStartInfo()
        startInfo.FileName <- executable
        for a in args do
          startInfo.ArgumentList.Add(a)
        startInfo.RedirectStandardOutput <- true
        startInfo.RedirectStandardError <- true
        startInfo.UseShellExecute <- false
        startInfo.CreateNoWindow <- true
        use p = new Process()
        p.StartInfo <- startInfo
        p.Start() |> ignore

        let outTask = Task.WhenAll([|
          p.StandardOutput.ReadToEndAsync();
          p.StandardError.ReadToEndAsync()
        |])

        do! p.WaitForExitAsync() |> Async.AwaitTask
        let! out = outTask |> Async.AwaitTask
        return {
          ExitCode = p.ExitCode;
          StandardOutput = out.[0];
          StandardError = out.[1]
        }
      }
    let executeShellCommand command =
       executeCommand "/usr/bin/env" [ "-S"; "bash"; "-c"; command ]

    let dockerStart =
      let r = executeShellCommand "docker run -d --name orbit --rm -p8085:8085 -eCLICOLOR_FORCE=1 cr.orbit.dev/sdu/filesync-server:latest" |> Async.RunSynchronously
      if r.ExitCode = 0 then
        printfn "%s" r.StandardOutput
      else
        eprintfn "%s" r.StandardError
        Environment.Exit(r.ExitCode)
    
     [<StructuredFormatDisplay("test")>]
    type docker() =
        let Engines = [{
            stopString = "stop orbit"
            startString = "run -d --name orbit --rm -p8085:8085 -eCLICOLOR_FORCE=2 cr.orbit.dev/sdu/filesync-server:latest"
            address = "http://localhost:8085/"
        };
        {
            stopString = "stop orbit1"
            startString = "run -d --name orbit1 --rm -p8084:8085 -eCLICOLOR_FORCE=2 cr.orbit.dev/sdu/filesync-server:latest"
            address = "http://localhost:8084/"
        };
          {
            stopString = "stop orbit2"
            startString = "run -d --name orbit2 --rm -p8083:8085 -eCLICOLOR_FORCE=2 cr.orbit.dev/sdu/filesync-server:latest"
            address = "http://localhost:8083/"
        };
          {
            stopString = "stop orbit3"
            startString = "run -d --name orbit3 --rm -p8082:8085 -eCLICOLOR_FORCE=2 cr.orbit.dev/sdu/filesync-server:latest"
            address = "http://localhost:8083/"
        };
          {
            stopString = "stop orbit4"
            startString = "run -d --name orbit4 --rm -p8081:8085 -eCLICOLOR_FORCE=2 cr.orbit.dev/sdu/filesync-server:latest"
            address = "http://localhost:8083/"
        };
          {
            stopString = "stop orbit5"
            startString = "run -d --name orbit5 --rm -p8080:8085 -eCLICOLOR_FORCE=2 cr.orbit.dev/sdu/filesync-server:latest"
            address = "http://localhost:8083/"
        };
        ]
        
        let mutable counter = 0
        member __.GetCurrent =
            Engines.[counter]
        member __.Next =
            let _  = Shell.Exec( "docker" ,  Engines.[counter].stopString) 
            
            let s  = Shell.Exec( "docker" , Engines.[counter].startString)   
            counter <- counter+1
            if (counter = 6)
            then
                counter <- 0
            
             

   
       
    let dockerRestart =
      let r = executeShellCommand "docker restart orbit" |> Async.RunSynchronously
      if r.ExitCode = 0 then
        printfn "%s" r.StandardOutput
      else
        eprintfn "%s" r.StandardError
        Environment.Exit(r.ExitCode)
        
    let dockerStop =
      let r = executeShellCommand "docker stop orbit" |> Async.RunSynchronously
      if r.ExitCode = 0 then
        printfn "%s" r.StandardOutput
      else
        eprintfn "%s" r.StandardError
        Environment.Exit(r.ExitCode)