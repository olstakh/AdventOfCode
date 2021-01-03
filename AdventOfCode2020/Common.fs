module Common

open System

let applyFst f (a, b) = (f a, b)
let applySnd f (a, b) = (a, f b)

let applyToListHead (head::tail) f =
    (f head)::tail

let addToLastOrAppend add deflt lst line =
    if (String.IsNullOrEmpty(line))
    then deflt::lst // append
    else applyToListHead lst (add line) // add to last

let listWithOneItemChanged lst idx v =
    lst |> List.mapi(fun ind u -> if ind = idx then v else u)

let (++) (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)

let GenerateSome f = Seq.unfold(fun x -> Some(x, f x))
let FindFirstWhen valid = Seq.pairwise >> Seq.find valid >> fst

let Flatten x =
    let r = Array2D.length1 x
    let c = Array2D.length2 x
    [
        for i = 0 to r-1 do
            for j = 0 to c-1 do
                yield Array2D.get x i j
    ]
    
type Coord =
    {
        X : int
        Y : int
    } with
        static member (+) (c1, c2) = { X = c1.X + c2.X; Y = c1.Y + c2.Y }
        static member (*) (c, scale) = { X = c.X * scale; Y = c.Y * scale}
        static member Origin = { X = 0; Y = 0 }
        static member Manhattan c = abs c.X + abs c.Y
        
type Coord3D =
    {
        X : int
        Y : int
        Z : int
    } with
        static member (+) (c1, c2) = { X = c1.X + c2.X; Y = c1.Y + c2.Y; Z = c1.Z + c2.Z }
        static member GetX me = me.X
        static member GetY me = me.Y
        static member GetZ me = me.Z
        static member Origin = { X = 0; Y = 0; Z = 0 }

type Coord4D =
    {
        X : int
        Y : int
        Z : int
        W : int
    } with
        static member (+) (c1, c2) = { X = c1.X + c2.X; Y = c1.Y + c2.Y; Z = c1.Z + c2.Z; W = c1.W + c2.W }
        static member GetX me = me.X
        static member GetY me = me.Y
        static member GetZ me = me.Z
        static member GetW me = me.W
        static member Origin = { X = 0; Y = 0; Z = 0; W = 0 }

let AddOrUpdateValue (key, value) = Map.remove key >> Map.add key value
let AddOrUpdateValues m = Seq.foldBack AddOrUpdateValue m