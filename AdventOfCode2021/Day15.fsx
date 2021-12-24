module Day15 =

    let input =
        "Input.txt"
        |> System.IO.File.ReadAllLines

    let rows = Array.length input
    let cols = String.length input.[0]

    let Solve scaleFactor =
        let riskDp = Array2D.create (scaleFactor * rows + 1) (scaleFactor * cols + 1) -1

        let rec Move = function
            | nothing when nothing |> Set.isEmpty -> ()
            | toVisit ->
                let (dist, (r, c)) as first = Set.minElement toVisit

                let currentValue = (int input.[r % rows].[c % cols] - int '0')
                let currentValueShift = r / rows + c / cols
                let currentValue' = (currentValue + currentValueShift - 1) % 9 + 1
                riskDp.[r + 1, c + 1] <- dist + currentValue' 

                let InBounds (r, c) = 0 <= r && r < scaleFactor * rows && 0 <= c && c < scaleFactor * cols
                let AlreadyVisited (r, c) = riskDp.[r + 1, c + 1] <> -1
                let PlanToVisit (dist, (r, c)) =
                    toVisit
                    |> Set.exists(fun (dist', (r', c')) -> r = r' && c = c' && dist > dist')

                [(-1, 0); (+1, 0); (0, -1); (0, +1)] 
                |> List.map(fun (dr, dc) -> (r + dr, c + dc))
                |> List.filter InBounds
                |> List.filter (AlreadyVisited >> not)
                |> List.map(fun (dr, dc) -> (dist + currentValue', (dr, dc))) 
                |> List.filter(PlanToVisit >> not)
                |> Set.ofList
                |> Set.union toVisit
                |> Set.remove first
                |> Move

        (0, (0, 0)) |> Set.singleton |> Move
        riskDp.[scaleFactor * rows, scaleFactor * cols] - riskDp.[1, 1]

    module Task1 =
        let Answer = Solve 1

    module Task2 =
        let Answer = Solve 5
