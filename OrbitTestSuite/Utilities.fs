namespace OrbitTestSuite.Utilities
open OrbitTestSuite.InMemoryModel
open OrbitTestSuite.Models

module Utilities = 
    
    let rec compareFilesMetadata (xl:List<APIModels.metadata>) (yl:List<inMemoryModels.inMemoryFile>) = 
        match xl, yl with 
        | [], [] -> true
        | x::xs, y::ys -> (x.id = y.metadata.id) && (x.name = y.metadata.name) && (x.parentId = y.metadata.parentId) && (x.timestamp = y.metadata.timestamp) && (x.version = y.metadata.version) && (x.versionChanged = y.metadata.versionChanged) && compareFilesMetadata xs ys
        | _ -> false