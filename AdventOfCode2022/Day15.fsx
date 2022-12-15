#load "Scripts\\ParserLibrary.fsx"
#load "Scripts\\Utils.fsx"
open Utils
open ParserLibrary

module Day15 =

    let Parser =
        let coordParser = pstring "x=" >>. pint .>> pstring ", y=" .>>. pint
        let parser =
            pstring "Sensor at "
            >>. coordParser
            .>> pstring ": closest beacon is at "
            .>>. coordParser

        ParseLine parser

    let data = Input() |> Array.map Parser
    let beacons = data |> Array.map snd |> Array.distinct

    let GetSegments y =
        data
        |> Array.map(fun ((xs, ys), (xb, yb)) ->
            let len = abs (xs - xb) + abs (ys - yb) - abs(y - ys)
            xs - len, xs + len)
        |> Array.filter(fun (x1, x2) -> x1 <= x2)
        |> Array.sortBy fst
        |> Array.fold (fun xLst (x1, x2) ->
            match (xLst) with
            | [] -> (x1, x2)::xLst
            | (x1', x2')::tail when x1 <= x2' -> (x1', max x2' x2)::tail
            | (x1', x2')::tail -> (x1,x2)::(x1',x2')::tail
        ) []
        |> List.sortBy fst

    module Task1 =
        let y = 2000000
        let Answer =
            let segments = GetSegments y
            let beacons' =
                beacons
                |> Array.filter(fun (bx, by) -> by = y && segments |> List.exists(fun (x1, x2) -> x1 <= bx && bx <= x2))
                |> Array.length            
            segments 
            |> List.sumBy(fun (x1, x2) -> x2 - x1 + 1)
            |> fun res -> res - beacons'

    module Task2 =
        let FindGap (minCoord, maxCoord) (y, xLst) =
            let combine (currx1, currx2) = function
                | (x1, x2) when x2 < currx1 || x1 > currx2 -> (currx1, currx2)
                | (x1, x2) when x1 <= currx1 && currx1 <= x2 -> (x2 + 1, currx2)
                | (x1, x2) -> (currx1, x1 - 1)
            xLst
            |> List.fold combine (minCoord, maxCoord)
            |> function
                | (x1, x2) when x1 = x2 -> Some (x1, y)
                | _ -> None

        let y = 4000000
        let Coords =
            [| 0 .. y |]
            |> Array.Parallel.map GetSegments
            |> Array.indexed
            |> Array.pick (FindGap (0, y))
        let Answer = int64(fst Coords) * int64(y) + int64(snd Coords)

    0