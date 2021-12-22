module Day11 =

    let Adjacent = [
        (-1, -1); (-1, 0); (-1, +1);
        (0, -1); (0, +1);
        (+1, -1); (+1, 0); (+1, +1)
    ]

    let simulateStep (arr : int[,]) =

        let grid = arr |> Array2D.map (fun v -> v % 10 + 1)
        
        let rows = grid |> Array2D.length1
        let cols = grid |> Array2D.length2
        let inBounds (r, c) = 0 <= r && r < rows && 0 <= c && c < cols
        let shouldIncrease (r, c) = grid.[r, c] < 10
        
        let rec Flash = function
            | [] -> grid
            | (r, c)::rest ->
                let newFlashes =
                    Adjacent
                    |> List.map(fun (dr, dc) -> (r + dr, c + dc))
                    |> List.filter inBounds
                    |> List.filter shouldIncrease
                newFlashes |> List.iter(fun (r, c) -> grid.[r, c] <- grid.[r, c] + 1)
                newFlashes |> List.filter(shouldIncrease >> not) |> List.append rest |> Flash
        
        grid
        |> Array2D.mapi(fun row col v -> (row, col), v)
        |> Seq.cast<(int * int) * int>
        |> Seq.where(fun (_, v) -> v = 10)
        |> Seq.map(fun ((row, col), _) -> (row, col))
        |> Seq.toList
        |> Flash

    let input =
        "Input.txt"
        |> System.IO.File.ReadAllLines

    let initialGrid = Array2D.init (input.Length) (input.[0].Length) (fun r c -> int input.[r].[c] - int '0')

    module Task1 =
        let Answer =
            initialGrid
            |> Array2D.copy
            |> Seq.unfold(fun g -> Some(g, simulateStep g))
            |> Seq.take 101
            |> Seq.collect(Seq.cast<int>)
            |> Seq.filter((=)10)
            |> Seq.length

    module Task2 =
        let Answer =
            initialGrid
            |> Array2D.copy
            |> Seq.unfold(fun g -> Some(g, simulateStep g))
            |> Seq.takeWhile(Seq.cast<int> >> Seq.exists(fun v -> v < 10))
            |> Seq.length