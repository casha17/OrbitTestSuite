namespace OrbitTestSuite.Test


module test =
    type Tree =
        | Branch of string  * Tree list
        | Leaf of string
    type FileInfo = {name:string; fileSize:int}
    type DirectoryInfo = {name:string; dirSize:int}
    