module BlackFox.MirrorOfFrised.Sb

open System
open System.Text

let inline appendIndent (indent: int) (sb: StringBuilder) =
    sb.Append(String(' ', indent)) |> ignore

let inline appendLine (sb: StringBuilder) =
    sb.AppendLine() |> ignore

let inline appendChar (c: char) (sb: StringBuilder) =
    sb.Append c |> ignore

let inline appendCharI (indent: int) (c: char) (sb: StringBuilder) =
    sb |> appendIndent indent
    sb.Append c |> ignore

let inline append (s: string) (sb: StringBuilder) =
    sb.Append s |> ignore

let inline appendI (indent: int) (s: string) (sb: StringBuilder) =
    sb |> appendIndent indent
    sb.Append s |> ignore
