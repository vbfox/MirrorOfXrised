namespace BlackFox.MirrorOfFrised

open System
open System.Text
open FSharp.Reflection

open Sb

module Mirror =

    type MirrorFunc = obj -> int -> StringBuilder -> unit

    [<CustomComparison; CustomEquality; Struct>]
    type ComparableType = {
        Type: Type
    }
    with
        interface IComparable<ComparableType> with
            member this.CompareTo { Type = t } =
                let assemblyCompare = this.Type.Assembly.FullName.CompareTo(t.Assembly.FullName)
                if assemblyCompare = 0 then
                    this.Type.FullName.CompareTo(t.FullName)
                else
                    assemblyCompare

        interface IComparable with
            member this.CompareTo other =
                match other with
                  | :? ComparableType as other ->
                      (this :> IComparable<_>).CompareTo other
                  | _ -> 1

        interface IEquatable<ComparableType> with
            member this.Equals other =
                (this :> IComparable<_>).CompareTo other = 0

        override this.Equals obj =
            match obj with
              | :? ComparableType as other ->
                  (this :> IEquatable<_>).Equals other
              | _ -> false

        override this.GetHashCode () =
            hash (this.Type.Assembly.FullName, this.Type.FullName)

    [<Struct>]
    type MirrorCache = {
        Mirrors: Map<ComparableType, MirrorFunc>
    }

    type private BuilderFunc = Type -> MirrorCache-> (MirrorFunc * MirrorCache)

    let private addToCache (t: ComparableType) (f: MirrorFunc) (cache: MirrorCache) =
        let newMap = cache.Mirrors |> Map.add t f
        { cache with Mirrors = newMap }

    let private getOrAddToCache (t: Type) (builder: BuilderFunc) (cache: MirrorCache) =
        let comparableType: ComparableType = { Type = t }
        match cache.Mirrors |> Map.tryFind comparableType with
        | Some mirror ->
            mirror, cache
        | None ->
            // Trampolines are present only for recursive types, as the builder function hasn't finished they can't get
            // it directly so they get a trampoline instead that will resolve to the correct function later
            let mutable mirrorForTrampoline: MirrorFunc option = None
            let trampoline value indent sb =
                match mirrorForTrampoline with
                | Some mirror -> mirror value indent sb
                | None -> failwithf "Trampoline for %s used before being filled" t.FullName

            let cacheWithTrampoline = addToCache comparableType trampoline cache
            let realMirror, newCache = builder t cacheWithTrampoline
            mirrorForTrampoline <- Some realMirror

            let finalCache = addToCache comparableType realMirror newCache
            realMirror, finalCache

    let rec private buildDirect (_t: Type) (cache: MirrorCache): MirrorFunc * MirrorCache =
        let f value _indent sb =
            sb |> append (sprintf "%O" value)

        f, cache

    and private buildString (_t: Type) (cache: MirrorCache): MirrorFunc * MirrorCache =
        let f (value: obj) _indent sb =
            let s = value :?> string
            sb |> appendChar '"'
            // TODO: Escape string
            sb |> append s
            sb |> appendChar '"'

        f, cache

    and private buildRecord (t: Type) (cache: MirrorCache): MirrorFunc * MirrorCache =
        let fields = FSharpType.GetRecordFields t
        let fieldBuilders, cache =
            fields |> Array.fold (fun (builders, cache) f ->
                let getter o = f.GetValue(o)
                let fieldMirror, cache = addMirrorToCache f.PropertyType cache
                let builder = f.Name, getter, fieldMirror
                builder :: builders, cache
            ) ([], cache)

        let fieldBuilders = fieldBuilders |> List.rev |> Array.ofSeq

        let f value indent sb =
            sb |> appendChar '{'
            let indent = if fields.Length > 1 then indent else 0
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

        f, cache

    and private buildDu (t: Type) (cache: MirrorCache): MirrorFunc * MirrorCache =
        let tagReader = FSharpValue.PreComputeUnionTagReader(t)
        let cases = FSharpType.GetUnionCases(t)

        let caseFieldBuilders, cache =
            cases |> Array.fold (fun (previousCaseFields, previousCache) c ->
                let fields = c.GetFields()
                let caseFields, cache =
                    fields |> Array.fold (fun (builders, cache) f ->
                        let getter o = f.GetValue(o)
                        let fieldMirror, cache = addMirrorToCache f.PropertyType cache
                        let builder = getter, fieldMirror
                        builder :: builders, cache
                    ) ([], previousCache)

                let caseFields = caseFields |> List.rev |> Array.ofSeq
                caseFields :: previousCaseFields, cache
            ) ([], cache)

        let caseFieldBuilders = caseFieldBuilders |> List.rev |> Array.ofSeq

        let f value indent sb =
            let tag = tagReader value
            let case = cases.[tag]
            let fieldBuilders = caseFieldBuilders.[tag]
            if fieldBuilders.Length > 0 then
                sb |> appendChar '('
            sb.Append(t.FullName + "." + case.Name) |> ignore
            for fieldGetter, fieldBuilder in fieldBuilders do
                sb |> appendChar ' '
                let fieldValue = fieldGetter value
                fieldBuilder fieldValue indent sb

            if fieldBuilders.Length > 0 then
                sb |> appendChar ')'

        f, cache

    and private buildAnyMirrorCore (t: Type) (cache: MirrorCache) : MirrorFunc * MirrorCache =
        if FSharpType.IsUnion t then
            buildDu t cache
        else if FSharpType.IsRecord t then
            buildRecord t cache
        else if t.IsPrimitive then
            buildDirect t cache
        else if t = typeof<string> then
            buildString t cache
        else
            failwithf "Not supported %s" t.FullName

    and addMirrorToCache (t: Type) (cache: MirrorCache) : MirrorFunc * MirrorCache =
        getOrAddToCache t buildAnyMirrorCore cache

    let buildMirror<'a> () =
        let cache: MirrorCache = { Mirrors = Map.empty }
        let f, _ = addMirrorToCache (typeof<'a>) cache
        f

    let mirror<'a> (value: 'a) =
        let sb = StringBuilder(2048)
        buildMirror<'a> () (box value) 0 sb |> ignore
        sb.ToString()
