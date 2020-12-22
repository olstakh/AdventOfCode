module Day13

let Solve1 (fileContent : string[]) =
    let myDepartureTime = fileContent.[0] |> int
    let buses =
        fileContent.[1].Split(',')
        |> Array.filter ((<>)"x")
        |> Array.map int

    let waitingTime busId = (busId - myDepartureTime % busId) % busId

    let bestBus = buses |> Array.minBy waitingTime

    bestBus * (waitingTime bestBus)

open Modulo

let Solve2 (fileContent : string[]) =
    fileContent.[1].Split(',')
    |> Array.indexed
    |> Array.filter(snd >> (<>)"x")
    |> Array.map (fun (ind, n) -> (int64 ind, int64 n))
    |> Array.sortBy snd
    |> Array.map(fun (remainder, prime) -> (remainder -~% prime, prime))
    |> SolveGarner
    
    
    