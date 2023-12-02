#load "Scripts\\Utils.fsx"
open Utils

module Day1 =

    let Positions (line : string) ((s : string), v) =
        match (line.IndexOf(s), line.LastIndexOf(s)) with
            | (i, j) when i >= 0 && j >= 0 -> [(i, v); (j, v)]
            | (_, _) -> []

    let SolveLine mapping (line : string) =
        let values = mapping |> Map.toList |> Seq.collect (Positions line) |> Seq.sortBy fst |> Seq.map snd
        let first = Seq.head values
        let last = Seq.last values
        first * 10 + last

    let Solve mapping =
        Input()
        |> Array.map (SolveLine mapping)
        |> Array.sum

    module Task1 =
        let mapping =
            [
                "1", 1; "2", 2; "3", 3; "4", 4; "5", 5; "6", 6; "7", 7; "8", 8; "9", 9
            ] |> Map.ofList
        let Answer() = Solve mapping

    module Task2 =
        let mapping =
            [
                "1", 1; "2", 2; "3", 3; "4", 4; "5", 5; "6", 6; "7", 7; "8", 8; "9", 9
                "one", 1; "two", 2; "three", 3; "four", 4; "five", 5; "six", 6; "seven", 7; "eight", 8; "nine", 9
            ] |> Map.ofList

        let Answer() = Solve mapping

Day1.Task1.Answer()
Day1.Task2.Answer()