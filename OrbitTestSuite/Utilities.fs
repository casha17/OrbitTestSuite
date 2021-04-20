namespace OrbitTestSuite.Utilities
open OrbitTestSuite.API
open OrbitTestSuite.Models.Model
open FsCheck
module Utilities = 
    let uploadFileModel apiModel content userId =
         let s = apiModel.users |> List.find (fun user -> user.userId = userId)
         let a = apiModel.users |> List.where (fun user -> user.userId <> userId)
         let fileId = if userId = "100" then "2" else "3"
         let qw = s.files |> List.find (fun file -> file.fileId = fileId)
         let b = {qw with content = content; fileVersion = qw.fileVersion+1} 
         let x = {s with files = b::[] }
         let v = a@x::[]
         {apiModel with users = v}
   
    let uploadFileModelWithBug apiModel content userId =
         let s = apiModel.users |> List.find (fun user -> user.userId = userId)
         let a = apiModel.users |> List.where (fun user -> user.userId <> userId)
         let fileId = if userId = "100" then "2" else "3"
         let qw = s.files |> List.find (fun file -> file.fileId = fileId)
         let b = if(qw.fileVersion<50) then {qw with content = content; fileVersion = qw.fileVersion+1} else {qw with content = ""; fileVersion = qw.fileVersion+1} // Silly bug
         let x = {s with files = b::[] }
         let v = a@x::[]
         {apiModel with users = v}
        
    let uploadFileSut apiModel content userId =
        let user = apiModel.users |> List.find (fun e-> e.userId = userId)
        let file = user.files |> List.head
        let currentFileversion = file.fileVersion-1
        let currentFileid = file.fileId
        let s = API.fileupload content userId currentFileid  (string currentFileversion) (string currentFileversion) "637479675580000000" 
        true.ToProperty()
        
    let downloadFileModel model userId =
         let currentFileId = if userId = "100" then "2" else "3"
         let localRes = model.users |> List.find (fun e -> e.userId = userId)
         let s = localRes.files |> List.find (fun e-> e.fileId =  currentFileId)
         s
    let downloadFileSut userId =
        let currentFileId = if userId = "100" then "2" else "3"
        let Apires = (API.downloadFile userId currentFileId)
        Apires