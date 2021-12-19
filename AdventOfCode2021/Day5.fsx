module Day5 =

    type Segment =
        {
            x1 : int
            x2 : int
            y1 : int
            y2 : int
        }
        with
            static member X2 s = s.x2
            static member Y2 s = s.y2
            static member Parse (s : string) =
                let [| start; finish |] = s.Split(" -> ")
                let [| x1; y1 |] = start.Split(',') |> Array.map int
                let [| x2; y2 |] = finish.Split(',') |> Array.map int
                { x1 = x1; x2 = x2; y1 = y1; y2 = y2 }

    let Solve filteredSegments =
        let maxX = filteredSegments |> Array.map(fun s -> max (s.x1) (s.x2)) |> Array.max
        let maxY = filteredSegments |> Array.map(fun s -> max (s.y1) (s.y2)) |> Array.max

        let Plane = Array2D.zeroCreate (maxY + 1) (maxX + 1)

        for segment in filteredSegments do
            let yLength = segment.y2 - segment.y1
            let xLength = segment.x2 - segment.x1
            let length = max (abs yLength) (abs xLength)
            let dy = if length = 0 then 0 else yLength / length
            let dx = if length = 0 then 0 else xLength / length
            for d in 0 .. length do
                let x = segment.x1 + dx * d
                let y = segment.y1 + dy * d
                Plane.[y, x] <- Plane.[y, x] + 1

        Plane
        |> Array2D.map(fun v -> if v > 1 then 1 else 0)
        |> Seq.cast<int>
        |> Seq.sum 

    let segments =
        "Input.txt"
        |> System.IO.File.ReadAllLines
        |> Array.map (Segment.Parse)

    module Task1 =
        let Answer =
            segments
            |> Array.filter(fun segment -> segment.x1 = segment.x2 || segment.y1 = segment.y2)
            |> Solve

    module Task2 =
        let Answer =
            segments
            |> Array.filter(fun segment -> segment.x1 = segment.x2 || segment.y1 = segment.y2 || abs(segment.y2 - segment.y1) = abs(segment.x2 - segment.x1))
            |> Solve