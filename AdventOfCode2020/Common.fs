module Common

open System

let addToLastOrAppend (head::tail as lst) line =
    if (String.IsNullOrEmpty(line))
    then ""::lst // append
    else (head + " " + line)::tail // add to last

let listWithOneItemChanged lst idx v =
    lst |> List.mapi(fun ind u -> if ind = idx then v else u)

let (+) (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)

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
    