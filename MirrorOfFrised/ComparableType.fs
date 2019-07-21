namespace BlackFox.MirrorOfFrised

open System

[<CustomComparison; CustomEquality; Struct>]
type ComparableType = {
    Type: Type
}
with
    static member Create(t: Type): ComparableType =
        { Type = t }

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
