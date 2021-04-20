namespace OrbitTestSuite.ConfigureModel

open OrbitTestSuite.API
open OrbitTestSuite.InMemoryModel.inMemoryModels
open OrbitTestSuite.Models

open OrbitTestSuite.InMemoryModel
open type inMemoryModels.user
open type inMemoryModels.inMemoryFile
module ConfigureModel = 





    let rec getFileContent (input:List<APIModels.metadata>) (userId:string)  = match input with
        | [] -> []
        | el::els ->  
            let result = (API.downloadFile userId (string el.id))
            
            {
                users = [
                    {
                        files = [{fileVersion = 1;content = result.data;fileId = (string el.id)}]
                        userId =  userId
                    }
                ]
            }::getFileContent els userId
            

    let intializeInMemoryModel =
        let listFilesResult = API.listFiles "100"
        let s =
            listFilesResult.data.fileList |> List.map (fun e ->
                let s = API.downloadFile "100" (string e.id)
                {
                     files = [{fileVersion = 1; content = s.data; fileId = (string e.id)};]
                     userId = "100"
                }
               )
            
        {users = s}
        
        (*
        printf "INIT"
        let model = inMemoryModels.inMemoryModel()
        model.setUser TempUser
        let listFilesResult = API.listFiles (string model.GetUser.id)
        model.appendFiles (getFileContent listFilesResult.data.fileList (string model.GetUser.id))
        model.appendDirectories (API.directoryStructure (string model.GetUser.id)).data
        model.setDirectoryVersions (listFilesResult.data.directoryVersions)
        model
        *)