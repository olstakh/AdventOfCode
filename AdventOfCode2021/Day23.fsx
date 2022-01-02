open Microsoft.FSharp.Reflection

module Day23 =
    // Adjacent matrix that can lookup distance and full path between verticies 
    type AdjacencyMatrix (matrixSize : int) =
        let Size = matrixSize

        member val Distances = Array2D.init Size Size (fun r c -> if r = c then 0 else 1000)
        member val Paths = Array2D.create Size Size (-1)

        member x.AddPath cost (u, v) =
                x.Distances[u, v] <- cost
                x.Distances[v, u] <- cost    

        member x.CalculateOptimalPath() =
            for ch = 0 to Size - 1 do
                for ot = 0 to Size - 1 do
                    for ku = 0 to Size - 1 do
                        let alternativeDistance = x.Distances[ot, ch] + x.Distances[ch, ku]
                        if  x.Distances[ot, ku] > alternativeDistance then
                            x.Distances[ot, ku] <- alternativeDistance
                            x.Paths[ot, ku] <- ch  

        member x.GetDistance (u, v) = x.Distances[u, v]
        member x.GetFullPath = function
            | (u, v) when x.Paths[u, v] = -1 -> [v]
            | (u, v) -> (x.GetFullPath (u, x.Paths[u, v])) @ (x.GetFullPath (x.Paths[u, v], v))

    type Amphipod = | A | B | C | D
        with
            static member TryParse =    function | 'A' -> Some A | 'B' -> Some B | 'C' -> Some C | 'D' -> Some D | _ -> None
            static member EnergyCost =  function | A -> 1 | B -> 10 | C -> 100 | D -> 1000
            static member All =         [| A; B; C; D |] //FSharpType.GetUnionCases typeof<Amphipod> |> Array.map (fun case -> FSharpValue.MakeUnion(case, [||]) :?> Amphipod)
            static member Index x =     Amphipod.All |> Array.findIndex((=)x)
            static member Total =       Amphipod.All |> Array.length

    type BurrowPosition =
        | Hallway of int
        | Sideroom of Amphipod * int
        with
            static member TotalHallwayPositions = 1 + Amphipod.Total + 1 + 1
            static member TotalPositionsWith roomSize = BurrowPosition.TotalHallwayPositions + Amphipod.Total * roomSize
            static member IsSideroomFor x = function
                | Sideroom (x' ,_) when x = x' -> true
                | _ -> false
            static member IsInHallway = function | Hallway _ -> true | _ -> false
            static member TrySideroomDepth = function
                | Hallway _ -> None
                | Sideroom (_, depth) -> Some depth
            static member ToSideroom room depth = Sideroom (room, depth)
            static member WithDepth depth room = Sideroom (room, depth)
            static member TryConnectToSideroom = function
                | Hallway x -> Amphipod.All |> Array.tryItem (x / 2 - 1) |> Option.map (BurrowPosition.WithDepth 0)
                | _ -> None

    type AmphipodPosition =
        {
            Amphipod : Amphipod
            Position : BurrowPosition
        }
        with
            static member GetAmphipod x = x.Amphipod
            static member GetPosition x = x.Position
            static member IsInRoomFor x = AmphipodPosition.GetPosition >> BurrowPosition.IsSideroomFor x
            static member InDestinationRoom x = x.Position |> BurrowPosition.IsSideroomFor (x.Amphipod)
            static member WithPosition position x = { Amphipod = x; Position = position }

    type BurrowMap (roomSize : int) =
        let RoomSize = roomSize
        let Matrix = AdjacencyMatrix (BurrowPosition.TotalPositionsWith RoomSize)
        let PositionToIndex = function
            | Hallway h -> h
            | Sideroom (a, r) -> BurrowPosition.TotalHallwayPositions + (Amphipod.Index a) * RoomSize + r
        let IndexToPosition = function
            | hallway when hallway < BurrowPosition.TotalHallwayPositions -> Hallway hallway
            | someRoom ->   let someRoom = someRoom - BurrowPosition.TotalHallwayPositions
                            let room = someRoom / roomSize
                            let subroom = someRoom % roomSize
                            Sideroom (Amphipod.All[room], subroom)

        let PositionsToIndex (u, v) = (PositionToIndex u, PositionToIndex v)
        let AddManyPathWith cost = List.map PositionsToIndex >> List.iter (Matrix.AddPath cost)

        let InitMatrix() =       
            let connectTo u v = (u, v)
            let connectToRoom x = List.map (connectTo (Sideroom (x, 0)))
            let connectToRoomByIdx (idx, room) = connectToRoom room [Hallway (idx + 1); Hallway (idx + 2)]
            let connectSubrooms room =
                [0 .. RoomSize - 1]
                |> List.map(fun depth -> Sideroom (room, depth))
                |> List.pairwise
            
            let hallwayConnections = [1 .. BurrowPosition.TotalHallwayPositions - 2] |> List.map Hallway |> List.pairwise
            let leftCorner = (Hallway 0, Hallway 1)
            let rightCorner = (Hallway (BurrowPosition.TotalHallwayPositions - 2), Hallway (BurrowPosition.TotalHallwayPositions - 1))

            let entrances =
                Amphipod.All
                |> Array.toList
                |> List.indexed
                |> List.collect connectToRoomByIdx

            let subrooms =
                Amphipod.All
                |> Array.toList
                |> List.collect connectSubrooms

            hallwayConnections |> AddManyPathWith 2
            [leftCorner; rightCorner] |> AddManyPathWith 1
            entrances |> AddManyPathWith 2
            subrooms |> AddManyPathWith 1

        do InitMatrix()
        do Matrix.CalculateOptimalPath()

        member x.GetFullPath = PositionsToIndex >> Matrix.GetFullPath >> List.map IndexToPosition
        member x.GetDistance = PositionsToIndex >> Matrix.GetDistance
        member x.GetFullPathFrom u v = x.GetFullPath (u, v)    

    let Solve roomSize initialPositions =

        let map = BurrowMap(roomSize)
        let mutable Memoization = Map.empty
        let Memoize key value = 
            Memoization <- Map.add key value Memoization
            value
        
        let rec OrderAmphipods positions =    

            let checkPositions f = f positions
            let occupied position =  checkPositions (List.map AmphipodPosition.GetPosition >> List.contains position)
            let cleanRoom amphipod = checkPositions (List.filter (AmphipodPosition.IsInRoomFor amphipod) >> List.forall AmphipodPosition.InDestinationRoom)
            let isFinal = checkPositions (List.forall AmphipodPosition.InDestinationRoom)

            let goHallwayWhileCan x = map.GetFullPathFrom x >> List.takeWhile (occupied >> not) >> List.filter (BurrowPosition.IsInHallway)
            let possibleMoves = function
                | { Amphipod = _; Position = Sideroom (x, _)  } when cleanRoom x -> []
                | { Amphipod = amphipod; Position = Hallway _ } when not (cleanRoom amphipod) -> []
                | { Amphipod = amphipod; Position = Hallway x } ->
                    [roomSize - 1 .. -1 .. 0]
                    |> List.map (BurrowPosition.ToSideroom amphipod)
                    |> List.tryFind (map.GetFullPathFrom (Hallway x) >> List.forall(occupied >> not))
                    |> Option.map List.singleton
                    |> Option.defaultValue []
                | { Amphipod = _; Position = position } -> (goHallwayWhileCan position (Hallway 0)) @ (goHallwayWhileCan position (Hallway 6))
        
            let tryMakeMoves x =
                let idx = checkPositions (List.findIndex ((=)x))
                let tryMakeMove move =
                    let dist = map.GetDistance (x.Position, move) //BurrowPosition.Distance 0 (List.item idx positions) move
                    let energyCost = dist * (Amphipod.EnergyCost (x.Amphipod))
                    positions
                    |> List.updateAt idx { x with Position = move }
                    |> OrderAmphipods
                    |> Option.map((+)energyCost)               
                possibleMoves x |> List.choose tryMakeMove

            let evaluateWith = function   
                | _ when isFinal -> Some 0
                | p when Memoization.ContainsKey p -> Memoization[p]
                | p -> p |> List.collect tryMakeMoves |> List.sort |> List.tryHead |> Memoize p

            evaluateWith positions

        OrderAmphipods initialPositions

    let SolveRaw (arr : string[]) =    
        let positions  =
            let parseChar (r, c) = Amphipod.TryParse >> Option.map(fun amphipod -> AmphipodPosition.WithPosition (Sideroom (Amphipod.All[(c - 3) / 2], r - 2)) amphipod)
            let parseRow (row, line : string) = line.ToCharArray() |> Array.indexed |> Array.choose(fun (col, ch) -> parseChar (row, col) ch)

            arr
            |> Array.indexed
            |> Array.collect parseRow
            |> Array.toList

        let maxRoomDepth = positions |> List.choose(AmphipodPosition.GetPosition >> BurrowPosition.TrySideroomDepth) |> List.max
        let roomSize = maxRoomDepth + 1
        Solve roomSize positions

    let data =
        "Input.txt"
        |> System.IO.File.ReadAllLines

    module Task1 =
        let Answer = data |> SolveRaw

    module Task2 =
        let toAdd = [|
            "  #D#C#B#A#"
            "  #D#B#A#C#" 
        |]
        let Answer = data |> Array.insertManyAt 3 toAdd |> SolveRaw
