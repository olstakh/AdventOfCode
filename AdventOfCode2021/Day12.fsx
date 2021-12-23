module Day12 =

    let input =
        "Input.txt"
        |> System.IO.File.ReadAllLines

    let graph =
        let addEdges gr (vFrom, vTo) =
            let addEdge newEdge = Option.fold List.append [newEdge] >> Some
            gr
            |> Map.change vFrom (addEdge vTo)
            |> Map.change vTo (addEdge vFrom)
            
        input
        |> Array.map(fun line -> line.Split('-'))
        |> Array.map(fun arr -> (arr.[0], arr.[1]))
        |> Array.fold addEdges Map.empty

    let isStart (cave : string) = cave = "start"
    let isFinal (cave : string) = cave = "end"
    let isSmall (cave : string) = cave.ToLowerInvariant() = cave
    let isBig   (cave : string) = cave.ToUpperInvariant() = cave

    let Solve reenterCount =

        let rec CountPath reentersLeft visitedCaves = function
            | finalCave when isFinal finalCave -> 1
            | smallCave when isSmall smallCave && reentersLeft = 0 && Set.contains smallCave visitedCaves -> 0
            | currentCave ->
                let visitedCaves' = Set.add currentCave visitedCaves
                let reentersLeft' =
                    if isSmall currentCave && Set.contains currentCave visitedCaves
                    then reentersLeft - 1 else reentersLeft

                graph
                |> Map.find currentCave
                |> List.filter (isStart >> not) // can't go back to the starting cave
                |> List.sumBy (CountPath reentersLeft' visitedCaves')

        CountPath reenterCount Set.empty "start"

    module Task1 =
        let Answer = Solve 0

    module Task2 =
        let Answer = Solve 1
