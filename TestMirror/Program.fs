// Learn more about F# at http://fsharp.org

open BlackFox.MirrorOfFrised
open System

type SomeRecord = {
    Foo: string
    N: string
    Bar: int option
    I: int
    NN: double option
    Next: SomeDu
    Lst: int list
}

and SomeDu =
| Foo
| Bar of SomeRecord

(*
let tv: SomeDu =
    (Bar {
        Foo = "Hello"
        N = ""
        Bar = (Some -5)
        I = 666
        NN = None

    })
*)

[<EntryPoint>]
let main argv =
    let v = Bar {
        Foo = "Hello"
        N = null
        Bar = Some -5
        I = 666
        NN = None
        Next = Bar {
            Foo = "Hello"
            N = null
            Bar = Some -5
            I = 666
            NN = None
            Next = Foo
            Lst = [1; 2; 3]
        }
        Lst = []
    }
    let s = Mirror.mirror v
    printfn "%s" s
    let x = Microsoft.FSharp.Core.option.None
    0 // return an integer exit code
