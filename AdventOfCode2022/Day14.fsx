#load "Scripts\\ParserLibrary.fsx"
#load "Scripts\\Utils.fsx"
open Utils
open ParserLibrary

module Day14 =
    type Rock =
        {
            x1 : int; y1 : int
            x2 : int; y2 : int
        }
        with
            static member IsRock (x, y) = function
                | rock when rock.x1 = rock.x2 -> x = rock.x1 && min (rock.y1) (rock.y2) <= y && y <= max (rock.y1) (rock.y2)
                | rock when rock.y1 = rock.y2 -> y = rock.y1 && min (rock.x1) (rock.x2) <= x && x <= max (rock.x1) (rock.x2)
            static member IsRocks rocks (x, y) = List.exists (Rock.IsRock (x, y)) rocks
            static member Highest rock = max (rock.y1) (rock.y2)
            static member Parser =
                sepBy (pint .>> pchar ',' .>>. pint) (pstring " -> ")
                |>> (List.pairwise >> List.map(fun ((x1, y1), (x2, y2)) -> { x1 = x1; y1 = y1; x2 = x2; y2 = y2 }))

    let Fall (x, y) =
        [
            (x, y + 1)
            (x - 1, y + 1)
            (x + 1, y + 1)
        ]
 
    let Solve rocks =
        let highestRock = (rocks |> List.map(Rock.Highest) |> List.max)

        let rec SandFallDown sand (x, y) = 
            let rec fall = function
                | (x, y) when y > highestRock -> None
                | (x, y) ->
                    (x, y)
                    |> Fall
                    |> List.filter (Rock.IsRocks rocks >> not)
                    |> List.filter (fun s -> sand |> Set.contains s |> not)
                    |> List.tryHead
                    |> function
                        | None -> Some (x, y)
                        | Some (x', y') -> fall (x', y')

            fall(x, y)

        let rec PourSand iteration sand =
            match (SandFallDown sand (500, 0)) with
                | None -> sand
                | Some (500, 0) -> Set.add (500, 0) sand
                | Some (x, y) ->
                    if (iteration % 100 = 0) then
                        printfn "%d" (Set.count sand)
                    PourSand (iteration + 1) (Set.add (x, y) sand)

        PourSand 0 (Set.empty)
        |> Seq.length

    let rocks = Input() |> Array.toList |> List.collect(ParseLine (Rock.Parser))
    
    module Task1 =
        let Answer = Solve rocks

    module Task2 =
        let highestY = rocks |> List.map(Rock.Highest) |> List.max
        let rocks' = { x1 = -1000000; y1 = highestY + 2; x2 = +1000000; y2 = highestY + 2 }::rocks
        let Answer = Solve rocks'