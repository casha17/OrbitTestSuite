module OrbitTestSuite.CounterTest

open FsCheck
open OrbitTestSuite.directoryget.directoryget

module InitialTest =
    
    
    
    type HashTable() =
        let mutable list = initalValue
        member __.Get = list
        override __.ToString() = sprintf "Counter=%A" list

    let spec =
        let Get = { new Command<HashTable,list<info>>() with
                override __.RunActual (model) = model
                override __.RunModel m = m
                override __.Post(counter, m) = counter.Get = m |@ sprintf "model: %A <> %A" m  counter.Get
                override __.ToString() = sprintf "Get"  }
        
        { new ICommandGenerator<HashTable,list<info>> with
            member __.InitialActual = HashTable()
            member __.InitialModel = bodyContent
            member __.Next model =  Gen.elements [Get;] }