module Day3

let private Traverse (map : string[]) (stepR, stepC) =
    let (mapR, mapC) = (map |> Array.length, map.[0].Length)

    Seq.unfold(fun (r, c) -> Some((r, c), (r + stepR, c + stepC))) (0, 0)
    |> Seq.takeWhile(fun (r, c) -> r < mapR)
    |> Seq.map(fun (r, c) -> map.[r].[c % mapC])
    |> Seq.filter(fun ch -> ch = '#')
    |> Seq.length

let Solve1 (fileContent : string[]) =
    Traverse fileContent (1, 3)

let Solve2 (fileContent : string[]) =
    let ans1 = Traverse fileContent (1, 1) |> int64
    let ans2 = Traverse fileContent (1, 3) |> int64
    let ans3 = Traverse fileContent (1, 5) |> int64
    let ans4 = Traverse fileContent (1, 7) |> int64
    let ans5 = Traverse fileContent (2, 1) |> int64

    ans1 * ans2 * ans3 * ans4 * ans5
