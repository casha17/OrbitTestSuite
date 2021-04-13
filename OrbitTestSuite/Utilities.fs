namespace OrbitTestSuite.Utilities

open HttpFs.Client
open Hopac

module Utilties =
    
    let getUrl parameter userId = "http://localhost:8085/" + parameter + "?userId=" +  userId
    let GetRequests parameter userId  =
        Request.createUrl Get (getUrl parameter userId) 
        |> Request.responseAsString
        