namespace BlackFox.MirrorOfFrised

open System
open System.Text
open FSharp.Reflection

open System
open System.Collections
open System.Collections.Generic
open System.Globalization
open System.Reflection
open Sb

type IValueVisitor = interface
    abstract member VisitString: string -> unit
    abstract member VisitByte: Byte -> unit
    abstract member VisitInt16: Int16 -> unit
    abstract member VisitInt32: Int32 -> unit
    abstract member VisitInt64: Int64 -> unit
    abstract member VisitUInt16: UInt16 -> unit
    abstract member VisitUInt32: UInt32 -> unit
    abstract member VisitUInt64: UInt64 -> unit
    abstract member VisitDecimal: decimal -> unit
    abstract member VisitGuid: Guid -> unit
    abstract member VisitUnknown: obj -> unit

    abstract member RecordStart: Type -> fields : PropertyInfo[]-> unit
    abstract member RecordMember: PropertyInfo -> unit
    abstract member RecordEnd: Type -> unit

    abstract member DiscriminatedUnionStart: Type -> tag : int -> case : UnionCaseInfo -> caseFields : PropertyInfo[] -> unit
    abstract member DiscriminatedUnionField: PropertyInfo -> unit
    abstract member DiscriminatedUnionEnd: Type -> unit

    abstract member ListStart: Type -> unit
    abstract member ListEnd: Type -> unit

    abstract member ArrayStart: Type -> unit
    abstract member ArrayEnd: Type -> unit
end

module FSharpTypeVisit =
    type ValueGuide = obj -> IValueVisitor -> unit

    module private PrimitiveGuides =
        let byteGuide = fun (o: obj) (visitor: IValueVisitor) -> visitor.VisitByte(o :?> Byte)
        let int16Guide = fun (o: obj) (visitor: IValueVisitor) -> visitor.VisitInt16(o :?> Int16)
        let int32Guide = fun (o: obj) (visitor: IValueVisitor) -> visitor.VisitInt32(o :?> Int32)
        let int64Guide = fun (o: obj) (visitor: IValueVisitor) -> visitor.VisitInt64(o :?> Int64)
        let intU16Guide = fun (o: obj) (visitor: IValueVisitor) -> visitor.VisitUInt16(o :?> UInt16)
        let intU32Guide = fun (o: obj) (visitor: IValueVisitor) -> visitor.VisitUInt32(o :?> UInt32)
        let intU64Guide = fun (o: obj) (visitor: IValueVisitor) -> visitor.VisitUInt64(o :?> UInt64)
        let decimalGuide = fun (o: obj) (visitor: IValueVisitor) -> visitor.VisitDecimal(o :?> Decimal)
        let stringGuide = fun (o: obj) (visitor: IValueVisitor) -> visitor.VisitString(o :?> String)
        let guidGuide = fun (o: obj) (visitor: IValueVisitor) -> visitor.VisitGuid(o :?> Guid)

    [<Struct>]
    type GuideCache = {
        Guides: Map<ComparableType, ValueGuide>
    }

    let cacheWithPrimitives =
        let guides =
            Map.empty
            |> Map.add (ComparableType.Create(typeof<Byte>)) PrimitiveGuides.byteGuide
            |> Map.add (ComparableType.Create(typeof<Int16>)) PrimitiveGuides.int16Guide
            |> Map.add (ComparableType.Create(typeof<Int32>)) PrimitiveGuides.int32Guide
            |> Map.add (ComparableType.Create(typeof<Int64>)) PrimitiveGuides.int64Guide
            |> Map.add (ComparableType.Create(typeof<UInt16>)) PrimitiveGuides.intU16Guide
            |> Map.add (ComparableType.Create(typeof<UInt32>)) PrimitiveGuides.intU32Guide
            |> Map.add (ComparableType.Create(typeof<UInt64>)) PrimitiveGuides.intU64Guide
            |> Map.add (ComparableType.Create(typeof<Decimal>)) PrimitiveGuides.byteGuide
            |> Map.add (ComparableType.Create(typeof<String>)) PrimitiveGuides.byteGuide
            |> Map.add (ComparableType.Create(typeof<Guid>)) PrimitiveGuides.byteGuide

        { Guides = guides }

    type private PropertyGetter = obj -> obj

    [<Struct>]
    type private FieldInfo = {
        Property: PropertyInfo
        Guide: ValueGuide
    }

    [<Struct>]
    type private DiscriminatedUnionCaseInfo = {
        Case: UnionCaseInfo
        Fields: PropertyInfo[]
        FieldInfos : List<FieldInfo>
    }

    type private GuideBuilder = Type -> GuideCache-> (ValueGuide * GuideCache)

    let private addToCache (t: ComparableType) (f: ValueGuide) (cache: GuideCache) =
        let newMap = cache.Guides |> Map.add t f
        { cache with Guides = newMap }

    let private getOrAddToCache (t: Type) (builder: GuideBuilder) (cache: GuideCache) =
        let comparableType: ComparableType = { Type = t }
        match cache.Guides |> Map.tryFind comparableType with
        | Some mirror ->
            mirror, cache
        | None ->
            // Trampolines are present only for recursive types, as the builder function hasn't finished they can't get
            // it directly so they get a trampoline instead that will resolve to the correct function later
            let mutable mirrorForTrampoline: ValueGuide option = None
            let trampoline value visitor =
                match mirrorForTrampoline with
                | Some mirror -> mirror value visitor
                | None -> failwithf "Trampoline for %s used before being filled" t.FullName

            let cacheWithTrampoline = addToCache comparableType trampoline cache
            let realMirror, newCache = builder t cacheWithTrampoline
            mirrorForTrampoline <- Some realMirror

            let finalCache = addToCache comparableType realMirror newCache
            realMirror, finalCache

    let rec private buildRecordCore (t: Type) (fields: PropertyInfo[]) (fieldInfos: List<FieldInfo>) =
        fun value (visitor : IValueVisitor) ->
            visitor.RecordStart t fields
            for fieldInfo in fieldInfos do
                visitor.RecordMember(fieldInfo.Property)

                let fieldValue = fieldInfo.Property.GetValue(value)
                fieldInfo.Guide fieldValue visitor

            visitor.RecordEnd(t)

    and private buildRecord (t: Type) (cache: GuideCache): ValueGuide * GuideCache =
        let fields = FSharpType.GetRecordFields t
        let fieldsInfos = List<FieldInfo>()
        let mutable cache = cache

        for field in fields do
            let guide, newCache = getOrCreateGuide field.PropertyType cache
            cache <- newCache
            fieldsInfos.Add({ Property = field; Guide = guide; })

        let f = buildRecordCore t fields fieldsInfos

        f, cache

    and private buildDuCore (t: Type) (tagReader: obj -> int) (casesInfo: List<DiscriminatedUnionCaseInfo>)=
        fun value (visitor : IValueVisitor) ->
            let tag = tagReader value
            let caseInfo = casesInfo.[tag]

            visitor.DiscriminatedUnionStart t tag caseInfo.Case caseInfo.Fields

            for fieldInfo in caseInfo.FieldInfos do
                visitor.DiscriminatedUnionField fieldInfo.Property

                let fieldValue = fieldInfo.Property.GetValue(value)
                fieldInfo.Guide fieldValue visitor

            visitor.DiscriminatedUnionEnd t

    and private buildDu (t: Type) (cache: GuideCache): ValueGuide * GuideCache =
        let tagReader = FSharpValue.PreComputeUnionTagReader(t)
        let casesInfo = List<DiscriminatedUnionCaseInfo>()
        let mutable cache = cache

        for case in FSharpType.GetUnionCases(t) do
            let fields = case.GetFields()
            let fieldsInfo = List<FieldInfo>()
            for field in fields do
                let guide, newCache = getOrCreateGuide field.PropertyType cache
                cache <- newCache
                fieldsInfo.Add({ Property = field; Guide = guide; })

            casesInfo.Add({Case = case; Fields = fields; FieldInfos = fieldsInfo})

        let f = buildDuCore t tagReader casesInfo

        f, cache

    and private createComplexGuide (t: Type) (cache: GuideCache) : ValueGuide * GuideCache =
        if FSharpType.IsUnion t then
            buildDu t cache
        else if FSharpType.IsRecord t then
            buildRecord t cache
        else
            failwithf "Not supported %s" t.FullName

    and getOrCreateGuide (t: Type) (cache: GuideCache) : ValueGuide * GuideCache =
        getOrAddToCache t createComplexGuide cache

    let createGuide (t: Type) : ValueGuide =
        let result, _  = getOrCreateGuide t cacheWithPrimitives
        result

module Mirror =

    type MirrorVisitor() =
        let sb = StringBuilder(2048)
        let indentStack = Stack<int>()

        override x.ToString() =
            sb.ToString()

        interface IValueVisitor with
            member x.VisitString s =
                sb |> appendChar '"'
                // TODO: Escape string
                sb |> append s
                sb |> appendChar '"'

            member x.VisitByte b =
                sb |> append (b.ToString(CultureInfo.InvariantCulture))

            member x.VisitInt16 i =
                sb |> append (i.ToString(CultureInfo.InvariantCulture))

            member x.VisitInt32 i =
                sb |> append (i.ToString(CultureInfo.InvariantCulture))

            member x.VisitInt64 i =
                sb |> append (i.ToString(CultureInfo.InvariantCulture))

            member x.VisitUInt16 i =
                sb |> append (i.ToString(CultureInfo.InvariantCulture))

            member x.VisitUInt32 i =
                sb |> append (i.ToString(CultureInfo.InvariantCulture))

            member x.VisitUInt64 i =
                sb |> append (i.ToString(CultureInfo.InvariantCulture))

            member x.VisitDecimal i =
                sb |> append (i.ToString(CultureInfo.InvariantCulture))

            member x.VisitGuid g =
                sb |> append "(System.Guid.Parse(\""
                sb |> append (g.ToString())
                sb |> append "\"))"

            member x.VisitUnknown o =
                sb |> append "/* "
                sb |> append (o.GetType().FullName)
                sb |> append " = "
                sb |> append (o.ToString())
                sb |> append "*/"

            member x.RecordStart _t fields =
                sb |> append "{"
                if fields.Length > 0
                let indent = indentStack.Peek() + 4
                indentStack.Push(indent)

                sb.AppendLine()
            member x.RecordMember p = sb.Append("???") |> ignore
            member x.RecordEnd t = sb.Append("???") |> ignore

            member x.DiscriminatedUnionStart t tag case caseFields = sb.Append("???") |> ignore
            member x.DiscriminatedUnionField p = sb.Append("???") |> ignore
            member x.DiscriminatedUnionEnd t = sb.Append("???") |> ignore

            member x.ListStart t = sb.Append("???") |> ignore
            member x.ListEnd t = sb.Append("???") |> ignore

            member x.ArrayStart t = sb.Append("???") |> ignore
            member x.ArrayEnd t = sb.Append("???") |> ignore

    type MirrorFunc = obj -> int -> StringBuilder -> unit

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
