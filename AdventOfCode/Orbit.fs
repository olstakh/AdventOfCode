module Orbit

let graph = 
    System.IO.File.ReadAllLines("C:\Users\olstakh\Source\Repos\AdventOfCode\AdventOfCode\Input.txt")
    |> Array.map(fun t -> t.Split(')'))
    |> Array.collect(fun a -> [|(a.[0], a.[1]); (a.[1], a.[0])|])
    |> Array.toSeq
    |> Seq.groupBy fst
    |> Seq.map(fun (k, s) -> (k, s |> Seq.map snd))

let rec CountOrbits currentDepth node =
    match (graph |> Seq.tryFind(fun (k, _) -> k = node)) with
        | Some(_, nextNodes) -> currentDepth + (nextNodes |> Seq.sumBy(CountOrbits (currentDepth + 1)))
        | None -> currentDepth

let rec FindPath nodeTo nodeParent nodeFrom =
    match (graph |> Seq.find(fun (k, _) -> k = nodeFrom)) with
    | (destination, _) when destination = nodeTo -> 0
    | (_, nextNodes) when nextNodes |> Seq.filter((<>)(nodeParent)) |> Seq.length = 0 -> 1000000000
    | (_, nextNodes) -> (nextNodes |> Seq.filter((<>)(nodeParent)) |> Seq.map(FindPath nodeTo nodeFrom) |> Seq.min) + 1

CountOrbits 0 "COM"
FindPath "SAN" "" "YOU"

let a = Map.empty

