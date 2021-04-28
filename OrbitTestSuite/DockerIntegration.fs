namespace OrbitTestSuite.DockerIntegration
open System
open System.Diagnostics
open System.Threading.Tasks

module Docker =
    
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