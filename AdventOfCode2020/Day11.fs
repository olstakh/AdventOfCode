module Day11

open Common

type Position =
    | Floor | EmptySeat | OccupiedSeat
    with
        static member Parse = function
            | '.' -> Floor | 'L' -> EmptySeat | '#' -> OccupiedSeat
            | c -> failwithf "Unsupported position %c" c
        static member ToChar = function
            | Floor -> '.' | EmptySeat -> 'L' | OccupiedSeat -> '#'
        static member IsOccupied = function
            | OccupiedSeat -> true | _ -> false
        static member IsSeat = function
            | Floor -> false | _ -> true

type Plane = Plane of Position[,]
    with
        static member Rows = Array2D.length1
        static member Cols = Array2D.length2
        static member GetSeats (Plane x) = x
        static member tryPosition (Plane x) = function
            | (r, c) when 0 <= r && r < Plane.Rows x && 0 <= c && c < Plane.Cols x -> Some (Array2D.get x r c)
            | _ -> None
        static member trySeat x = Plane.tryPosition x >> Option.filter(Position.IsSeat)
        static member Parse (s : string[]) =
            let l1 = Array.length s
            let l2 = Array.get s 0 |> String.length
            
            Array2D.init l1 l2 (fun r c -> Array.get s r |> Seq.item c |> Position.Parse)
            |> Plane

        static member CountOccupied = Plane.GetSeats >> Flatten >> List.filter(Position.IsOccupied) >> List.length

let adjacentCoordsBy func plane coord =
    let adjacentCoordsDelta = List.allPairs [-1; 0; +1] [-1; 0; + 1] |> List.filter((<>)(0, 0))
    let adjacentCoordsDeltaInfinite = adjacentCoordsDelta |> List.map(fun initial -> Seq.unfold(fun c -> Some(c, c ++ initial)) initial)

    adjacentCoordsDeltaInfinite
    |> List.choose(
        Seq.map((++)coord)
        >> Seq.takeWhile(Plane.tryPosition plane >> Option.isSome)
        >> func
        >> Seq.tryPick(Plane.tryPosition plane >> Option.filter(Position.IsSeat))
    )    

let FillPassengers seatMapping getAdjacentSeats plane =
    plane
    |> Plane.GetSeats
    |> Array2D.mapi(fun r c seat -> seatMapping (getAdjacentSeats plane (r, c)) seat)
    |> Plane

let Solve seatRuleMapping getAdjacentSeats =
    Plane.Parse
    >> GenerateSome (FillPassengers seatRuleMapping getAdjacentSeats)
    >> FindFirstWhen(fun (plane, nextPlane) -> plane = nextPlane)
    >> Plane.CountOccupied

let Solve1(fileContent : string[]) =
    let getAdjacentSeats = adjacentCoordsBy (Seq.take 1)

    let seatRuleMapping adjSeats = function
        | EmptySeat when adjSeats |> List.filter (Position.IsOccupied) |> List.length = 0 -> OccupiedSeat
        | OccupiedSeat when adjSeats |> List.filter (Position.IsOccupied) |> List.length >= 4 -> EmptySeat
        | currentSeat -> currentSeat

    Solve seatRuleMapping getAdjacentSeats fileContent

let Solve2(fileContent : string[]) =
    let getAdjacentSeats = adjacentCoordsBy id

    let seatRuleMapping adjSeats = function
        | EmptySeat when adjSeats |> List.filter (Position.IsOccupied) |> List.length = 0 -> OccupiedSeat
        | OccupiedSeat when adjSeats |> List.filter (Position.IsOccupied) |> List.length >= 5 -> EmptySeat
        | currentSeat -> currentSeat
    
    Solve seatRuleMapping getAdjacentSeats fileContent
