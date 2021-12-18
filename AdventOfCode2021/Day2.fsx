#load "Scripts\\ParserLibrary.fsx"
open ParserLibrary

module Day2 =

    type SubmarineCommands =
        | Forward of int
        | Down of int
        | Up of int
        with
            static member Parse (s: string) =
                let pForward = pstring "forward" >>. spaces >>. pint |>> Forward
                let pUp = pstring "up" >>. spaces >>. pint |>> Up
                let pDown = pstring "down" >>. spaces >>. pint |>> Down

                match (run (choice [pForward; pUp; pDown]) s) with
                    | Success (parsedAction, _) -> parsedAction
                    | Failure (label, error, position) -> failwithf "Can't parse command %s. Label: %s. Error: %s. Position: %d" s label error position.column

    module Task1 =
        type SubmarinePosition =
            {
                Horizontal : int
                Depth : int
            }
            with
                static member Initial = { Horizontal = 0; Depth = 0; }
                static member ApplyCommand ship = function
                    | Forward x -> { ship with Horizontal = ship.Horizontal + x }
                    | Down x -> { ship with Depth = ship.Depth + x }
                    | Up x -> { ship with Depth = ship.Depth - x }
                static member GetHash ship =
                    int64(ship.Horizontal) * int64(ship.Depth)

        let Answer =
            "Input.txt"
            |> System.IO.File.ReadAllLines
            |> Array.map SubmarineCommands.Parse
            |> Array.fold SubmarinePosition.ApplyCommand SubmarinePosition.Initial
            |> SubmarinePosition.GetHash

    module Task2 =
        type SubmarinePosition =
            {
                Horizontal : int
                Depth : int
                Aim : int
            }
            with
                static member Initial = { Horizontal = 0; Depth = 0; Aim = 0 }
                static member ApplyCommand ship = function
                    | Down x -> { ship with Aim = ship.Aim + x }
                    | Up x -> { ship with Aim = ship.Aim - x }
                    | Forward x -> { ship with Horizontal = ship.Horizontal + x 
                                               Depth = ship.Depth + ship.Aim * x }
                static member GetHash ship =
                    int64(ship.Horizontal) * int64(ship.Depth)            

        let Answer =
            "Input.txt"
            |> System.IO.File.ReadAllLines
            |> Array.map SubmarineCommands.Parse
            |> Array.fold SubmarinePosition.ApplyCommand SubmarinePosition.Initial
            |> SubmarinePosition.GetHash
