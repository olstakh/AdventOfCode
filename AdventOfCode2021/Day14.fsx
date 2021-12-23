module Day14 =

    let RunSteps stepCount initialElements initialCompounds rules =
        
        let AddElementCount (element:string) countToAdd =
            let elementIdx = int element.[0] - int 'A'
            let currentCnt = Array.get initialElements elementIdx
            Array.set initialElements elementIdx (currentCnt + countToAdd)

        let RunStep currentCompounds =
            currentCompounds
            |> Map.toList
            |> List.collect(
                fun (key, value) ->
                    match (Map.tryFind key rules) with
                    | None -> [key, value]
                    | Some newCompound ->   do AddElementCount newCompound (int64 value)
                                            let updatedCompound = key.ToString().Insert(1, newCompound)
                                            [updatedCompound.Substring(0, 2), value;
                                             updatedCompound.Substring(1, 2), value]
            )
            |> List.groupBy fst
            |> List.map(fun (key, values) -> (key, values |> List.sumBy snd))
            |> Map.ofList

        let RunAllSteps = RunStep |> List.replicate stepCount |> List.reduce (>>)
        RunAllSteps initialCompounds

    let input =
        "Input.txt"
        |> System.IO.File.ReadAllLines

    let initial = input |> Array.head
    let rules =
        input
        |> Array.skip 2
        |> Array.map(fun line -> line.Split(" -> "))
        |> Array.map(fun arr -> (arr.[0], arr.[1]))
        |> Array.fold (fun rule (cFrom, cTo) -> Map.add cFrom cTo rule) Map.empty

    let initialCompounds =
        initial.ToCharArray()
        |> Array.pairwise
        |> Array.map(fun (a, b) -> string a + string b)
        |> Array.fold(fun compounds currentCompound -> compounds |> Map.change currentCompound (Option.fold (+) 1L >> Some)) Map.empty

    let Solve stepsToRun =
        let elements = Array.create 26 0L
        initial |> String.iter(fun ch -> elements.[int ch - int 'A'] <- elements.[int ch - int 'A'] + 1L)

        let _ = RunSteps stepsToRun elements initialCompounds rules

        let mostCommon = elements |> Array.max
        let leastCommonNonZero = elements |> Array.filter (fun cnt -> cnt > 0) |> Array.min

        mostCommon - leastCommonNonZero

    module Task1 =
        let Answer = Solve 10

    module Task2 =
        let Answer = Solve 40