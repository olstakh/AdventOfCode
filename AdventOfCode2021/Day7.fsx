module Day7 =

    let input =
        "Input.txt"
        |> System.IO.File.ReadAllText

    let data = input.Split(',') |> Array.map int |> Array.sort

    let Solve costFunction =
        let positionCost positionX =
            data |> Array.sumBy ( fun x -> abs (x - positionX) |> costFunction)

        let minX = data |> Array.min
        let maxX = data |> Array.max
        [minX .. maxX] |> List.map positionCost |> List.min // Lazy to write ternary or smth similar

    module Task1 =
        let Answer = Solve (fun dist -> dist)

    module Task2 =
        let Answer = Solve (fun dist -> dist * (dist + 1) / 2)