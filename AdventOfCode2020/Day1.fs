module Day1

let Solve1 (fileContent : string[]) =
    let numbers = fileContent |> Array.map int64
    let cnt = numbers |> Array.length

    // Sorry for O^2, want to get to harder problems fast :~)
    [
        for i = 0 to cnt - 1 do
            for j = i + 1 to cnt - 1 do
                if (numbers.[i] + numbers.[j] = 2020L) then yield (numbers.[i] * numbers.[j])
    ]
    |> List.exactlyOne

let Solve2 (fileContent : string[]) =
    let numbers = fileContent |> Array.map int64
    let cnt = numbers |> Array.length

    // Sorry for O^3, want to get to harder problems fast :~)
    [
        for i = 0 to cnt - 1 do
            for j = i + 1 to cnt - 1 do
                for k = j + 1 to cnt - 1 do
                    if (numbers.[i] + numbers.[j] + numbers.[k] = 2020L) then yield (numbers.[i] * numbers.[j] * numbers.[k])
    ]
    |> List.exactlyOne