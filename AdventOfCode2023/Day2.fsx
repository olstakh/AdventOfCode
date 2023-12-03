#load "Scripts\\Utils.fsx"
#load "Scripts\\ParserLibrary.fsx"

open Utils
open ParserLibrary

module Day2 =

    type Game =
        {
            Id : int
            CubeSets : CubeSet list
        }
        with
            static member Parser =
                pstring "Game " >>. pint .>> pstring ": " .>>. sepBy1 CubeSet.Parser (pstring "; ") |>> fun (id, cubeset) -> { Id = id; CubeSets = cubeset }
            static member GetCubes x = x.CubeSets
            static member GameId x = x.Id


    and CubeSet = CubeSet of (int * Color) list
        with
            static member Parser =
                sepBy1 (pint .>> pchar ' ' .>>. Color.Parser) (pstring ", ") |>> CubeSet

            static member GetCubes (CubeSet x) = x
            static member Power = CubeSet.GetCubes >> List.map fst >> List.reduce (*)

    and Color = Red | Green | Blue
        with 
            static member Parser =
                choice
                    [
                        pstring "red" >>. returnP Red
                        pstring "green" >>. returnP Green
                        pstring "blue" >>. returnP Blue
                    ]

    let Games =
        Input() |> Array.map (ParseLine (Game.Parser))

    module Task1 =
        // Checks if cubest x contains at least *cnt* cubes of color *color*
        let CubesContainedIn (CubeSet x) (cnt, color) =
                x |> List.tryFind(snd >> (=)color) |> Option.exists(fst >> (<=)cnt)

        // Checks if a cubeset can be contained inside cubeset *x*
        let ContainedIn x = CubeSet.GetCubes >> List.forall(CubesContainedIn x)

        // Checks if all given cubesets from the game can be contained in a *targetCubeSet*
        let IsGamePossible targetCubeSet = Game.GetCubes >> List.forall(ContainedIn targetCubeSet)

        let TargetCubeSet = CubeSet [(12, Red); (13, Green); (14, Blue)]
        let Answer =
            Games
            |> Array.filter(IsGamePossible TargetCubeSet)
            |> Array.sumBy(Game.GameId)

    module Task2 =
        // Combines two cubesets by picking max count for each color they contain
        let Combine (CubeSet x) =
            CubeSet.GetCubes >> List.append x >> List.groupBy snd >> List.map(snd >> List.maxBy fst) >> CubeSet
        let FindMinset = Game.GetCubes >> List.reduce Combine
        let Answer =
            Games
            |> Array.map FindMinset
            |> Array.sumBy (CubeSet.Power)
