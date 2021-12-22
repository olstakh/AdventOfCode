module Day9 =

    let input =
        "Input.txt"
        |> System.IO.File.ReadAllLines
        |> Array.map(fun line -> line.ToCharArray())

    let Adjacent (row, col) = [(row - 1, col); (row + 1, col); (row, col - 1); (row, col + 1)]

    let tryGetValue (row, col) =
        if (0 <= row && row < input.Length && 0 <= col && col < input.[0].Length)
            then Some input.[row].[col]
            else None

    let lowPoints =
        let rows = input.Length
        let cols = input.[0].Length
        [
            for row = 0 to rows - 1 do
                for col = 0 to cols - 1 do
                    let minAdjacent = (row, col) |> Adjacent |> List.choose tryGetValue |> List.min
                    if input.[row].[col] < minAdjacent then yield (row, col)
        ]

    module Task1 =
        let Answer =
            lowPoints
            |> List.choose tryGetValue
            |> List.sumBy(fun height -> int height - int '0' + 1)

    module Task2 =
        let rec Fill = function
            | outOfBounds when tryGetValue outOfBounds = None -> 0
            | visited when tryGetValue visited = Some 'x' -> 0
            | nine when tryGetValue nine = Some '9' -> 0
            | (row, col) ->
                input.[row].[col] <- 'x'
                1 + ((row, col) |> Adjacent |> List.sumBy Fill)

        let Answer =
            lowPoints
            |> List.map Fill
            |> List.sortDescending
            |> List.take 3
            |> List.reduce (*)