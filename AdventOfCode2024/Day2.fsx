#load "Scripts\\Utils.fsx"
open Utils

module Day2 =

    let IsSafeDiff diff =
        let maxD = List.max diff
        let minD = List.min diff
        (1 <= minD && maxD <= 3)
        ||
        (-1 >= maxD && minD >= -3)

    let GetReports() =
        Input()
        |> Array.map(fun s -> s.Split(' ', System.StringSplitOptions.RemoveEmptyEntries) |> Array.map int)

    let Solve isSafeFn =
        let getDiff = Array.pairwise >> Array.map(fun (a, b) -> a-b) >> Array.toList

        GetReports()
        |> Array.map getDiff
        |> Array.where isSafeFn
        |> Array.length

    module Task1 =
        let IsSafe diff =
            [
                yield diff
            ]
            |> List.exists IsSafeDiff

        let Answer() = Solve IsSafe

    module Task2 =
        let IsSafe diff =
            [
                yield diff // original array
                yield diff |> List.skip 1 // delete first
                yield! [0 .. diff.Length - 2]
                       |> List.map(fun ind -> (List.take ind diff) @ [diff[ind] + diff[ind + 1]] @ (List.skip(ind + 2) diff)) // delete in the middle
                yield diff |> List.rev |> List.skip 1 // delete last
            ]
            |> List.exists IsSafeDiff

        let Answer() = Solve IsSafe
        
Day2.Task1.Answer()
Day2.Task2.Answer()
