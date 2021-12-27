module Day21 =
    type Player =
        {
            Position : int
            Score : int
        }
        with
            static member FromPosition startingPosition = { Score = 0; Position = startingPosition }
            static member Move positionChange player =
                { player with Position = (player.Position + positionChange - 1) % 10 + 1 }
            static member UpdateScore player =
                { player with Score = player.Score + player.Position }
            static member MoveWithScoreUpdate player positionChange =
                player |> Player.Move positionChange |> Player.UpdateScore
           
    let input =
        "Input.txt"
        |> System.IO.File.ReadAllLines

    let getLastInt (s : string) = s.Substring(s.LastIndexOf(' ') + 1) |> int
    let player1 = input[0] |> getLastInt |> Player.FromPosition
    let player2 = input[1] |> getLastInt |> Player.FromPosition

    module Task1 =
        let scoreToReach = 1000
        let rec Play diceRolls p1 = function
            | p2 when p2.Score >= scoreToReach -> p1.Score * diceRolls
            | p2 ->
                p1
                |> Player.Move (diceRolls + 1)
                |> Player.Move (diceRolls + 2)
                |> Player.Move (diceRolls + 3)
                |> Player.UpdateScore
                |> Play (diceRolls + 3) p2

        let Answer = Play 0 player1 player2

    module Task2 =
        let scoreToReach = 21
        let mutable knownPositions = Map.empty
        
        let rec Play p1 = function
            | p2 when Map.containsKey (p1, p2) knownPositions -> knownPositions[(p1, p2)]
            | p2 when p2.Score >= scoreToReach -> (0L, 1L)
            | p2 ->
                let (p2wins, p1wins) =
                    [|
                        for move1 = 1 to 3 do
                            for move2 = 1 to 3 do
                                for move3 = 1 to 3 do
                                    move1 + move2 + move3
                    |]
                    |> Array.map (Player.MoveWithScoreUpdate p1 >> Play p2)
                    |> Array.reduce (fun (p2scoreTotal, p1scoreTotal) (p2score, p1score) -> (p2scoreTotal + p2score, p1scoreTotal + p1score))
                knownPositions <- knownPositions |> Map.add (p1, p2) (p1wins, p2wins)
                (p1wins, p2wins)

        let Answer = Play player1 player2 |> fun (p1wins, p2wins) -> max p1wins p2wins