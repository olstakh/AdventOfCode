module Day2

open System

let Solve1 (fileContent : string[]) =
    fileContent
    |> Array.map(fun s -> let [| minCnt; maxCnt; letter; password |] = s.Split([|'-'; ' '; ':'|], StringSplitOptions.RemoveEmptyEntries) in (minCnt |> int, maxCnt |> int, letter, password))
    |> Array.filter(fun (minCnt, maxCnt, letter, password) -> let t = password |> String.filter(fun ch -> ch = letter.[0]) |> String.length
                                                              minCnt <= t && t <= maxCnt)
    |> Array.length

let Solve2 (fileContent : string[]) =

    let Check (str : string) ch pos =
        if (0 <= pos && pos < str.Length) then
            str.[pos] = ch
        else false

    fileContent
    |> Array.map(fun s -> let [| firstPos; secondPos; letter; password |] = s.Split([|'-'; ' '; ':'|], StringSplitOptions.RemoveEmptyEntries) in ((firstPos |> int) - 1, (secondPos |> int) - 1, letter.[0], password))
    |> Array.filter(fun (firstPos, secondPos, letter, password) -> (Check password letter firstPos) <> (Check password letter secondPos))
    |> Array.length