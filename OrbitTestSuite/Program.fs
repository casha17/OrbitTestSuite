// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open OrbitTestSuite.API
open OrbitTestSuite.InMemoryModel.inMemoryModels
open OrbitTestSuite.TestSuite
open OrbitTestSuite.ConfigureModel
open OrbitTestSuite.test
// Define a function to construct a message to print



[<EntryPoint>]
let main argv =    
    //s.p
    testSuite.setupTestSuite
    (*
    let s = ConfigureModel.intializeInMemoryModel
    let api1 = API.fileupload "bar" (string s.GetUser.id) "2" (string (s.getCurrentFileVersion 2)) (string (s.getCurrentFileVersion 2))  "637479675580000000" 
    let local1 = s.WriteToLocalFile 2 "bar"
    let apiRes1 = API.downloadFile "100" "2"
    let api1 = API.fileupload "foo" (string s.GetUser.id) "2" (string (s.getCurrentFileVersion 2)) (string (s.getCurrentFileVersion 2))  "637479675580000000" 
    let local1 = s.WriteToLocalFile 2 "foo"
    let apiRes1 = API.downloadFile "100" "2"
    let api1 = API.fileupload "bar" (string s.GetUser.id) "2" (string (s.getCurrentFileVersion 2)) (string (s.getCurrentFileVersion 2))  "637479675580000000" 
    let local1 = s.WriteToLocalFile 2 "bar"
    let apiRes1 = API.downloadFile "100" "2"
    let api1 = API.fileupload "foo" (string s.GetUser.id) "2" (string (s.getCurrentFileVersion 2)) (string (s.getCurrentFileVersion 2))  "637479675580000000" 
    let local1 = s.WriteToLocalFile 2 "foo"
    let apiRes1 = API.downloadFile "100" "2"
    let api1 = API.fileupload "bar" (string s.GetUser.id) "2" (string (s.getCurrentFileVersion 2)) (string (s.getCurrentFileVersion 2))  "637479675580000000" 
    let local1 = s.WriteToLocalFile 2 "bar"
    let apiRes1 = API.downloadFile "100" "2"
    let api1 = API.fileupload "foo" (string s.GetUser.id) "2" (string (s.getCurrentFileVersion 2)) (string (s.getCurrentFileVersion 2))  "637479675580000000" 
    let local1 = s.WriteToLocalFile 2 "foo"
    let apiRes1 = API.downloadFile "100" "2"
    let api1 = API.fileupload "bar" (string s.GetUser.id) "2" (string (s.getCurrentFileVersion 2)) (string (s.getCurrentFileVersion 2))  "637479675580000000" 
    let local1 = s.WriteToLocalFile 2 "bar"
    let apiRes1 = API.downloadFile "100" "2"
    let api1 = API.fileupload "foo" (string s.GetUser.id) "2" (string (s.getCurrentFileVersion 2)) (string (s.getCurrentFileVersion 2))  "637479675580000000" 
    let local1 = s.WriteToLocalFile 2 "foo"
    let apiRes1 = API.downloadFile "100" "2"
    let api1 = API.fileupload "bar" (string s.GetUser.id) "2" (string (s.getCurrentFileVersion 2)) (string (s.getCurrentFileVersion 2))  "637479675580000000" 
    let local1 = s.WriteToLocalFile 2 "bar"
    
    let apiRes1 = API.downloadFile "100" "2"
    let localRes1 = s.getCurrentFileContent 2
    printf "%b" (apiRes1.data = localRes1)
    //printf "%A" s
    //printf "%A" s.GetFiles
    //printf "%s" (s.getCurrentFileContent 2)
    //s.WriteToLocalFile 2 "hej"
    //printf "\n \n"
    //printf "%A" s.GetFiles
    *)
    let s = ConfigureModel.intializeInMemoryModel
    
    let transform (apiModel:inMemory) =
        let s = apiModel.users |> List.map (fun user ->
                        if user.userId = "100" then { user with files = user.files |> List.map (fun e -> {e with content = ""; })} else user )
        {apiModel with users = s}

    let c = transform s
    printf "%s" ""
    0 // return an integer exit code