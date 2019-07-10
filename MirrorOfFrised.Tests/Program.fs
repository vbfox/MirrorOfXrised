// Learn more about F# at http://fsharp.org

open System

open Expecto

let tests = testList "x" []

[<EntryPoint>]
let main argv =
    runTestsWithArgs defaultConfig argv tests
