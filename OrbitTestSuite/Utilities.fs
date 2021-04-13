namespace OrbitTestSuite.Utilities
open OrbitTestSuite.Models

module Utilities = 

    let concatString x = 
        String.concat "" x
    
    let rec compareFilesMetadata (xl:List<APIModels.metadata>) (yl:List<inMemoryModels.inMemoryFile>) = 
        match xl, yl with 
        | [], [] -> true
        | x::xs, y::ys -> x.id = y.metadata.id && compareFilesMetadata xs ys
        | _ -> false