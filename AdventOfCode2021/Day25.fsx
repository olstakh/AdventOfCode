module Day25 =
    let RunStep (arr : char[,]) =

        let (rows, cols) = (Array2D.length1 arr, Array2D.length2 arr)

        let Move ch =
            let (dr, dc) = match (ch) with | '>' -> (0, +1) | 'v' -> (+1, 0)
            let moves = [
                for r = 0 to rows - 1 do 
                    for c = 0 to cols - 1 do
                        let (r', c') = ((r + dr) % rows, (c + dc) % cols)
                        if (arr[r', c'] = '.' && arr[r, c] = ch) then yield (r, c)
            ]
            moves |> List.iter(fun (r, c) -> arr[r, c] <- '.'; arr[(r + dr) % rows, (c + dc) % cols] <- ch)
            moves

        match (Move '>', Move 'v') with
            | ([], []) -> None
            | _ -> Some arr

    let input = "Input.txt" |> System.IO.File.ReadAllLines
    let inputData = Array2D.init (input.Length) (input[0].Length) (fun r c -> input[r][c])

    module LastTask =
        let Answer =
            Some inputData
            |> Seq.unfold (Option.bind(fun x -> Some (x, RunStep x)))
            |> Seq.length