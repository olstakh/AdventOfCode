module Day2 =

    type RoundResult =
        | Win | Lose | Draw
        with
            static member Score = function
                | Win -> 6 | Draw -> 3 | Lose -> 0
            static member Invert = function
                | Win -> Lose | Lose -> Win | Draw -> Draw
            static member Parse = function
                | 'X' -> Lose | 'Y' -> Draw | 'Z' -> Win
                | x -> failwithf "Unknown symbol %c" x

    type RPS =
        | Rock | Paper | Scisors
        with
            static member Parse = function
                | 'A' | 'X' -> Rock
                | 'B' | 'Y' -> Paper
                | 'C' | 'Z' -> Scisors
                | x -> failwithf "Unknown symbol %c" x
            static member Value = function
                | Rock -> 1 | Paper -> 2 | Scisors -> 3
            static member Play = function
                | (Rock, Paper) -> Lose
                | (Rock, Scisors) -> Win
                | (Paper, Scisors) -> Lose
                | (p1, p2) when p1 = p2 -> Draw
                | (p1, p2) -> RPS.Play (p2, p1) |> RoundResult.Invert
            static member FromResult = function
                | (x, Draw) -> x
                | (Rock, Win) -> Paper | (Rock, Lose) -> Scisors
                | (Paper, Win) -> Scisors | (Paper, Lose) -> Rock
                | (Scisors, Win) -> Rock | (Scisors, Lose) -> Paper

    let PlayForSecondPlayer (p1, p2) =
        RoundResult.Score(RPS.Play(p2, p1)) + (RPS.Value p2)

    let Solve parser =
        "Input.txt"
        |> System.IO.File.ReadAllLines
        |> Array.map (parser >> PlayForSecondPlayer)
        |> Array.sum

    module Task1 =
        let Parser (s:string) =
            let rps1 = RPS.Parse(s[0])
            let rps2 = RPS.Parse(s[2])
            (rps1, rps2)

        let Answer = Solve Parser 

    module Task2 =
        let Parser (s:string) =
            let rps1 = RPS.Parse(s[0])
            let outcome = RoundResult.Parse(s[2])
            let rps2 = RPS.FromResult (rps1, outcome)
            (rps1, rps2)

        let Answer = Solve Parser