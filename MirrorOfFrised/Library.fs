namespace BlackFox.MirrorOfFrised

open System
open System.Text
open FSharp.Reflection

module Mirror =
    open Sb
    type private MirrorFunc = obj -> int -> StringBuilder -> unit

    let rec private buildDirect (t: Type): MirrorFunc =
        fun value _indent sb ->
            sb |> append (sprintf "%O" value)

    and private buildString (_t: Type): MirrorFunc =
        fun value _indent sb ->
            let s = value :?> string
            sb |> appendChar '"'
            // TODO: Escape string
            sb |> append s
            sb |> appendChar '"'

    and private buildRecord (t: Type): MirrorFunc =
        let fields = FSharpType.GetRecordFields t
        let recordReader = FSharpValue.PreComputeRecordReader t
        let fieldBuilders =
            fields |> Array.map (fun f ->
                let getter o = f.GetValue(o)
                f.Name, getter, (buildAnyMirror f.PropertyType)
            )
        fun value indent sb ->
            sb |> appendChar '{'
            let indent = if fields.Length > 1 then indent else 0
            let fieldValues = recordReader value
            if fields.Length > 1 then
                sb |> appendLine

            let fieldIndent = indent + 4
            for fieldName, fieldGetter, fieldBuilder in fieldBuilders do
                sb |> appendIndent fieldIndent
                let fieldValue = fieldGetter value
                sb |> append fieldName
                sb |> append " = "
                fieldBuilder fieldValue fieldIndent sb
                if fields.Length > 1 then
                    sb |> appendLine

            sb |> appendIndent indent
            sb |> appendChar '}'

    and private buildDu (t: Type): MirrorFunc =
        let tagReader = FSharpValue.PreComputeUnionTagReader(t)
        let cases = FSharpType.GetUnionCases(t)
        let caseFieldBuilders =
            cases |> Array.map (fun c ->
                let fields = c.GetFields()
                fields |> Array.map (fun f ->
                    let getter o = f.GetValue(o)
                    getter, (buildAnyMirror f.PropertyType)
                ))
        fun value indent sb ->
            let tag = tagReader value
            let case = cases.[tag]
            let fieldBuilders = caseFieldBuilders.[tag]
            if fieldBuilders.Length > 0 then
                sb |> appendChar '('
            sb.Append(case.Name) |> ignore
            printfn "%A" (case.GetFields())
            for fieldGetter, fieldBuilder in fieldBuilders do
                sb |> appendChar ' '
                let fieldValue = fieldGetter value
                fieldBuilder fieldValue indent sb

            if fieldBuilders.Length > 0 then
                sb |> appendChar ')'

    and buildAnyMirror (t: Type) : MirrorFunc =
        if FSharpType.IsUnion t then
            buildDu t
        else if FSharpType.IsRecord t then
            buildRecord t
        else if t.IsPrimitive then
            buildDirect t
        else if t = typeof<string> then
            buildString t
        else
            failwithf "Not supported %s" t.FullName

    let buildMirror<'a> () =
        buildAnyMirror (typeof<'a>)

    let mirror<'a> (value: 'a) =
        let sb = StringBuilder(2048)
        buildMirror<'a> () (box value) 0 sb |> ignore
        sb.ToString()
