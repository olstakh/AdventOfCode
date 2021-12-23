module Day12 =

    type Fold =
        | Up of int
        | Left of int
        with
            static member Parse (line : string) =
                let instruction = line.Split(' ') |> Array.last
                match (instruction.Split('=')) with
                    | [| "x"; x |] -> Left (int x)
                    | [| "y"; y |] -> Up (int y)
                    | _ -> failwith "Unknown instruction"

    let ApplyFold (x, y) = function
        | Left alongX when x > alongX -> (2 * alongX - x, y)
        | Up   alongY when y > alongY -> (x, 2 * alongY - y)
        | _ -> (x, y)

    let ApplyManyFolds = Array.fold ApplyFold

    let input =
        "Input.txt"
        |> System.IO.File.ReadAllLines

    let dots =
        input
        |> Array.takeWhile(fun s -> s.StartsWith("fold") = false && s.Length > 0)
        |> Array.map(fun s -> s.Split(','))
        |> Array.map(fun arr -> (int arr.[0], int arr.[1]))

    let foldInstructions =
        input
        |> Array.skip (Array.length dots)
        |> Array.skip 1
        |> Array.map (Fold.Parse)

    module Task1 =
        let firstInstruction = foldInstructions |> Array.head
        let Answer =
            dots
            |> Array.map(fun dot -> ApplyFold dot (firstInstruction))
            |> Array.distinct
            |> Array.length

    module Task2 =
        let Visualize dots =
            let minX = dots |> Array.map fst |> Array.min
            let minY = dots |> Array.map snd |> Array.min
            let shiftedDots = dots |> Array.map(fun (x, y) -> (x - minX, y - minY))
            let maxX = shiftedDots |> Array.map fst |> Array.max
            let maxY = shiftedDots |> Array.map snd |> Array.max

            let grid = Array.init (maxY + 1) (fun _ -> Array.create (maxX + 1) '.')
            shiftedDots |> Array.iter(fun (x, y) -> grid.[y].[x] <- '#')

            grid |> Array.map (System.String.Concat)

        let Answer =
            dots
            |> Array.map(fun dot -> ApplyManyFolds dot foldInstructions)
            |> Array.distinct
            |> Array.sort
            |> Visualize
