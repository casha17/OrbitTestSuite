namespace OrbitTestSuite.Utilities
open OrbitTestSuite.InMemoryModel
open OrbitTestSuite.InMemoryModel.inMemoryModels
open OrbitTestSuite.Models
open OrbitTestSuite.Models.APIModels

module Utilities = 
    
    let rec compareFilesMetadata (xl:List<APIModels.metadata>) (yl:List<inMemoryModels.inMemoryFile>) = 
        match xl, yl with 
        | [], [] -> true
        | x::xs, y::ys -> (x.id = y.metadata.id) && (x.name = y.metadata.name) && (x.parentId = y.metadata.parentId) && (x.timestamp = y.metadata.timestamp) && (x.version = y.metadata.version) && (x.versionChanged = y.metadata.versionChanged) && compareFilesMetadata xs ys
        | _ -> false
        
    let compareFileMetadata (x: metadata) (yList: List<inMemoryFile>) =
        if (yList.Length = 0) then false
        else
            printf "Hejsa %A" yList.[0]
            let y = yList.[0].metadata
            let trip = (x.id = y.id) && (x.name = y.name) && (x.parentId = y.parentId) && (x.timestamp = y.timestamp) && (x.version = y.version) && (x.versionChanged = y.versionChanged)
            trip