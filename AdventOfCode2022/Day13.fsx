#load "Scripts\\ParserLibrary.fsx"
#load "Scripts\\Utils.fsx"
open Utils
open ParserLibrary

module Day13 =
    type Data =
        | Literal of int
        | Nested of Data list
        with
            static member Parser =
                let (pData, pDataRef) = createParserForwardedToRef<Data>()
                pDataRef.Value <- choice [
                    pint |>> Literal
                    pchar '[' >>. sepBy pData (pchar ',') .>> pchar ']' |>> Nested
                ]
                pData
            static member Parse = ParseLine (Data.Parser)
            static member Compare = function
                | (Literal x, Literal y) -> x.CompareTo(y)
                | (Literal x, Nested y) -> Data.Compare (Nested [Literal x], Nested y)
                | (Nested x, Literal y) -> Data.Compare (Nested x, Nested [Literal y])
                | (Nested [], Nested []) -> 0
                | (Nested [], Nested y) -> -1
                | (Nested x, Nested []) -> +1
                | (Nested (x::x'), Nested (y::y')) ->
                    match (Data.Compare (x, y)) with
                        | 0 -> Data.Compare (Nested x', Nested y')
                        | res -> res
    
    let rec Parse = function
        | [] -> []
        | ""::lst -> Parse lst
        | line1::line2::lst -> (Data.Parse line1, Data.Parse line2)::(Parse lst)

    let allData = Input() |> Array.toList |> Parse

    module Task1 =
        let Answer = 
            allData
            |> List.indexed
            |> List.filter(snd >> Data.Compare >> (=)(-1))
            |> List.sumBy(fst >> (+)1)

    module Task2 =
        let addition1 = Data.Parse "[[2]]"
        let addition2 = Data.Parse "[[6]]"

        let sortedData =
            allData
            |> List.append [addition1, addition2]
            |> List.collect(fun (l1, l2) -> [l1; l2])
            |> List.sortWith (fun a b -> Data.Compare (a, b))
            
        let res1 = sortedData |> List.findIndex ((=)addition1)
        let res2 = sortedData |> List.findIndex ((=)addition2)

        let Answer = (res1 + 1) * (res2 + 1)