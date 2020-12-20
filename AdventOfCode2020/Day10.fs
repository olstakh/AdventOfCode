module Day10

let private getAllJoltsSorted (fileContent : string[]) =
    let jolts = fileContent |> Array.map int
    let maxJolt = jolts |> Array.max
    let startJolt = 0
    let finalJolt = maxJolt + 3

    jolts
    |> Array.append [| startJolt; finalJolt |]
    |> Array.sort

let Solve1 (fileContent : string[]) =
    let joltDiff =
        fileContent
        |> getAllJoltsSorted
        |> Seq.pairwise
        |> Seq.map(fun (a, b) -> b - a)

    let diff1 = joltDiff |> Seq.filter((=) 1) |> Seq.length
    let diff3 = joltDiff |> Seq.filter((=) 3) |> Seq.length

    diff1 * diff3

let Solve2 (fileContent : string[]) =
    let allJolts = fileContent |> getAllJoltsSorted
    let lastJolt = allJolts |> Seq.last

    let WaysToConnectJolts prevWaysToConnectJolts currentJolt =
        let newConnection =
            prevWaysToConnectJolts
            |> List.sumBy(fun (prevJolt, ways) ->
                if (1 <= prevJolt - currentJolt && prevJolt - currentJolt <= 3)
                    then ways else 0L
            )

        [(currentJolt, newConnection)]
        |> List.append prevWaysToConnectJolts
        |> List.skip 1

    allJolts
    |> Seq.rev
    |> Seq.skip 1
    |> Seq.fold WaysToConnectJolts [(lastJolt, 0L); (lastJolt, 0L); (lastJolt, 1L)]
    |> List.last
    |> fun (firstJolt, waysToConnect) -> waysToConnect