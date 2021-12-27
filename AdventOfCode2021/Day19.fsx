module Day19 =

    type Coord3D =
        {
            X: int; Y: int; Z: int
        }
        with
            static member Parse (s:string) =
                let sSplit = s.Split(',') |> Array.map int
                { X = sSplit[0]; Y = sSplit[1]; Z = sSplit[2] }
            static member RotateX p = { p with Y = -p.Z; Z =  p.Y }
            static member RotateY p = { p with X =  p.Z; Z = -p.X }
            static member RotateZ p = { p with X = -p.Y; Y =  p.X }
            static member Rotate mask =
                // applies a function cnt amount of times
                let applyMany cnt = List.replicate cnt >> List.fold (>>) id
                let rotateX = (mask >>> 4) &&& 3
                let rotateY = (mask >>> 2) &&& 3
                let rotateZ = (mask >>> 0) &&& 3
                applyMany rotateX Coord3D.RotateX >>
                applyMany rotateY Coord3D.RotateY >>
                applyMany rotateZ Coord3D.RotateZ
            static member (-) (c1, c2) = { X = c1.X - c2.X; Y = c1.Y - c2.Y; Z = c1.Z - c2.Z }
            static member (+) (c1, c2) = { X = c1.X + c2.X; Y = c1.Y + c2.Y; Z = c1.Z + c2.Z }
            static member Manhattan c = abs (c.X) + abs (c.Y) + abs (c.Z)

    type Scanner =
        {
            Beacons : Coord3D list
            Position : Coord3D option
        }
        with
            static member AddBeacon scanner beacon = { scanner with Beacons = scanner.Beacons @ [beacon] }
            static member RotateBeacons scanner mask = { scanner with Beacons = scanner.Beacons |> List.map (Coord3D.Rotate mask) }
            static member AbsoluteBeacons scanner =
                match (scanner.Position) with
                    | None -> failwithf "Can't get absolute position of the beacons, because position of the scanner is unknown"
                    | Some position -> scanner.Beacons |> List.map((+)position)
            static member KnownPosition scanner = scanner.Position.IsSome

    let tryMatchAssumingSameRotation scanner1 scanner2 =
        let knownBeacons = scanner1 |> Scanner.AbsoluteBeacons |> Set.ofList
        let Verify = Scanner.AbsoluteBeacons >> Set.ofList >> Set.intersect knownBeacons >> Set.count >> (<=)12

        [
            for beacon1 in List.skip 11 scanner1.Beacons do
                for beacon2 in List.skip 11 scanner2.Beacons do
                    let potentialScanner2Pos = (beacon1 - beacon2) + scanner1.Position.Value
                    yield { scanner2 with Position = Some potentialScanner2Pos }
        ]
        |> List.distinct
        |> List.tryFind Verify

    let tryMatch scanner1 scanner2 =
        [|0 .. (1 <<< 6)|] // all rotation masks
        |> Array.map (Scanner.RotateBeacons scanner2)
        |> Array.distinct
        |> Array.Parallel.choose (tryMatchAssumingSameRotation scanner1)
        |> Array.tryHead
        |> Option.defaultValue scanner2

    let rec Solve (firstScanner::restOfScanners as knownScanners) = function
        | [] -> knownScanners
        | unknownScanners ->
            printfn "Remaining scanners with unknown position %d" (List.length unknownScanners)
            unknownScanners
            |> List.map (tryMatch firstScanner)
            |> List.partition (Scanner.KnownPosition)
            |> fun (firstScannerMatches', stillUnknownScanners') ->
                firstScanner :: (Solve (restOfScanners @ firstScannerMatches') stillUnknownScanners')

    let parseScanners currentScanners = function
        | newLine when String.length newLine = 0 -> currentScanners
        | newScanner when newScanner.StartsWith("---") -> { Beacons = []; Position = None }::currentScanners
        | newBeacon ->  let lastScanner::restOfScanners = currentScanners
                        (Scanner.AddBeacon lastScanner (Coord3D.Parse newBeacon) )::restOfScanners

    let knownScanner::unknownScanners =
        "Input.txt"
        |> System.IO.File.ReadAllLines
        |> Array.fold parseScanners []
        |> List.rev

    let resolvedScanners = Solve [{knownScanner with Position = Some {X = 0; Y = 0; Z = 0 }}] unknownScanners

    module Task1 =
        let Answer =
            resolvedScanners
            |> List.collect (Scanner.AbsoluteBeacons)
            |> List.distinct
            |> List.length

    module Task2 =
        let Answer =
            [
                for s1 in resolvedScanners do
                    for s2 in resolvedScanners do
                        yield (s2.Position.Value - s1.Position.Value) |> Coord3D.Manhattan
            ]
            |> List.max