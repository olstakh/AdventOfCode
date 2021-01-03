module Day6

open System

let private DistinctAnswers = 
    Seq.map(Set.ofSeq) >> Set.unionMany >> Set.count

let private CommonAnswers =
    Seq.map(Set.ofSeq) >> Set.intersectMany >> Set.count

let parseAnswers =
    Array.fold (Common.addToLastOrAppend (fun a b -> a + " " + b) "") [""]
    >> List.filter(String.IsNullOrEmpty >> not)
    >> List.map(fun s -> s.Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Array.toList)
    
let Solve1 (fileContent : string[]) =
    fileContent
    |> parseAnswers
    |> List.sumBy DistinctAnswers

let Solve2 (fileContent : string[]) =
    fileContent
    |> parseAnswers
    |> List.sumBy CommonAnswers
