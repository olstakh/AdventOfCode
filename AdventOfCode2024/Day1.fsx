#load "Scripts\\Utils.fsx"
open Utils

module Day1 =

    let GetLists() =
        Input()
        |> Array.map(fun s -> s.Split(' ', System.StringSplitOptions.RemoveEmptyEntries))
        |> Array.map(fun arr -> (int(arr[0]), int(arr[1])))
        |> Array.unzip

    module Task1 =
        let (left, right) = GetLists()

        let zipped =
            (left |> Array.sort)
            |> Array.zip <|
            (right |> Array.sort)

        let Answer() = zipped |> Array.sumBy(fun (x, y) -> abs (x-y))

    module Task2 =
        let (left, right) = GetLists()

        let rightMap = right |> Array.countBy id

        let Answer() =
            left |> Array.sumBy(fun x -> 
                (rightMap |> Array.tryFind(fst >> ( = )x) |> Option.map (snd >> ( * ) x) |> Option.defaultValue 0))

    
Day1.Task1.Answer()
Day1.Task2.Answer()

