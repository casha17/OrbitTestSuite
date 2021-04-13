namespace OrbitTestSuite.ConfigureModel

open OrbitTestSuite.API
open OrbitTestSuite.Models
open type inMemoryModels.user
open type inMemoryModels.inMemoryFile
module ConfigureModel = 


    let TempUser = {
            id = 100
            name = "Reader/Writer"
            initials = "rw"
        }


    let rec getFileContent (input:List<APIModels.metadata>) (userId:string)  = match input with
        | [] -> []
        | el::els ->  
            let result = (API.downloadFile userId (string el.id))
            {
                content = result.content
                metadata = el
            }::getFileContent els userId

    let intializeInMemoryModel = 
        let model = inMemoryModels.inMemoryModel()
        model.setUser TempUser
        let listFilesResult = API.listFiles (string model.GetUser.id)
        model.appendFiles (getFileContent listFilesResult.fileList (string model.GetUser.id))
        model.appendDirectories (API.directoryStructure (string model.GetUser.id))
        model.setDirectoryVersions (listFilesResult.directoryVersions)
        model
        