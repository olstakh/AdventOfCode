module Day10 =
    type Instruction =
        | Noop
        | Add of int
        with
            static member Parse (s:string) =
                let tokenized = s.Split(' ')
                if (tokenized[0] = "noop") then Noop else Add (int tokenized[1])

    let data = "Input.txt" |> System.IO.File.ReadAllLines |> Array.map Instruction.Parse

    let inline Solve accumulator (currentCycle, rValue, accumulatedValue) = function
        | Noop -> (currentCycle + 1, rValue, accumulatedValue + accumulator rValue currentCycle)
        | Add x -> (currentCycle + 2, rValue + x, accumulatedValue + accumulator rValue currentCycle + accumulator rValue (currentCycle + 1))

    module Task1 =
        let addValue x = function
            | cycle when cycle % 40 = 20  -> cycle * x
            | _ -> 0

        let (_, _, Answer) = data |> Array.fold (Solve addValue) (1, 1, 0)

    module Task2 =
        let addPixel x = function
            | cycle when abs (((cycle - 1) % 40) - x) <= 1 -> "#"
            | _ -> "."

        let (_, _, Answer) = data |> Array.fold (Solve addPixel) (1, 1, "")

        let rec Draw = function
            | s when String.length s <= 40 -> printfn "%s" s
            | s ->
                do printfn "%s" (s.Substring(0, 40))
                Draw (s.Substring(40))
    
        Draw Answer