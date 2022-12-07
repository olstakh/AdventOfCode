#load "Scripts\\Utils.fsx"
open Utils

module Day1 =

    let parse caloriesPerElf = function
        | newLine when String.length newLine = 0 -> 0::caloriesPerElf
        | calory -> let (currentCalories::restOfElves) = caloriesPerElf
                    (int calory + currentCalories)::restOfElves

    let elves =
        Input()
        |> Array.fold parse [0]
        |> List.sortDescending

    let Solve topElves =
        elves |> List.take topElves |> List.sum

    module Task1 =
        let Answer = Solve 1

    module Task2 =
        let Answer = Solve 3