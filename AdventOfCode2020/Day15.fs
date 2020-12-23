module Day15

open Common

let Play turns initialNumbers =
    let NextNumber (prevNumbers, lastNumber) turn =
        let nextNumber =
            prevNumbers
            |> Map.tryFind lastNumber
            |> Option.defaultValue (turn - 1)

        (prevNumbers |> AddOrUpdateValue (lastNumber, turn - 1), turn - nextNumber - 1)

    let lastNumber = initialNumbers |> List.last
    let prevNumbers =
        initialNumbers
        |> List.take (List.length initialNumbers - 1)
        |> List.indexed
        |> List.groupBy snd
        |> List.map (applySnd (List.map fst >> List.max >> (+)1))

    [List.length initialNumbers + 1 .. turns]
    |> List.fold NextNumber (Map.ofList prevNumbers, lastNumber)
    |> snd

let Solve turns = Array.map int >> Array.toList >> Play turns

let Solve1 (fileContent : string[]) =
    fileContent.[0].Split(',') |> Solve 2020

let Solve2 (fileContent : string[]) =
    fileContent.[0].Split(',') |> Solve 30000000 // ~ 2 min