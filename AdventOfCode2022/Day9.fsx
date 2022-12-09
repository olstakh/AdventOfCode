module Day9 = 
    type Direction = R | L | U | D
        with
            static member Parse = function | 'R' -> R | 'L' -> L | 'U' -> U | 'D' -> D
            static member MakeStep (r, c) = function
                | R -> (r, c + 1) | L -> (r, c - 1)
                | U -> (r - 1, c) | D -> (r + 1, c)

    let normalize x x' =
        match (x - x') with
        | 0 -> 0
        | d -> d / abs d
    
    let moveTail (headR, headC) = function
        | (r, c) when abs(r - headR) <= 1 && abs (c - headC) <= 1 -> (r, c)
        | (r, c) -> (r + normalize headR r, c + normalize headC c)

    let Parse (s:string) =
        let [|direction;steps|] = s.Split(' ')
        List.replicate (int steps) (Direction.Parse(direction[0]))

    let Move rope direction =
        let (head::tail) = rope
        let head' = Direction.MakeStep head direction
        List.scan moveTail head' tail

    let allMoves = "Input.txt" |> System.IO.File.ReadAllLines |> Array.toList |> List.collect Parse

    let Solve ropeLength =
        let rope = List.replicate ropeLength (0,0)
        allMoves
        |> List.scan Move rope
        |> List.map (List.last)
        |> List.distinct
        |> List.length

    module Task1 =
        let Answer = Solve 2

    module Task2 =
        let Answer = Solve 10