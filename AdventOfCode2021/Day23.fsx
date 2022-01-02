module Day23 =
    
    type AdjacencyMatrix (edges : (int*int)[]) =
        let maxVertice = (edges |> Array.map(fun (u, v) -> max u v) |> Array.max)
        let Size = maxVertice + 1

        let Distances = Array2D.init Size Size (fun r c -> if r = c then 0 else 1000)
        let Paths = Array2D.create Size Size (-1)

        let AddEdge (u, v) = Distances[u, v] <- 1; Distances[v, u] <- 1

        let CalculateOptimalPath() =
            for ch = 0 to Size - 1 do
                for ot = 0 to Size - 1 do
                    for ku = 0 to Size - 1 do
                        let alternativeDistance = Distances[ot, ch] + Distances[ch, ku]
                        if  Distances[ot, ku] > alternativeDistance then
                            Distances[ot, ku] <- alternativeDistance
                            Paths[ot, ku] <- ch  

        do edges |> Array.iter AddEdge
        do CalculateOptimalPath()

        member x.GetDistance (u, v) = Distances[u, v]
        member x.GetFullPath = function
            | (u, v) when Paths[u, v] = -1 -> [v]
            | (u, v) -> (x.GetFullPath (u, Paths[u, v])) @ (x.GetFullPath (Paths[u, v], v))

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
            static member TotalHallwayPositions = 1 + Amphipod.Total * 2 + 1 + 1            
            static member AllHallways = [| 0 .. BurrowPosition.TotalHallwayPositions - 1 |] |> Array.map Hallway

            static member ToSideroom room depth = Sideroom (room, depth)
            static member WithDepth depth room = Sideroom (room, depth)

            static member IsSideroomFor x = function
                | Sideroom (x' ,_) when x = x' -> true
                | _ -> false
            static member IsInHallway = function
                | Hallway _ -> true
                | _ -> false
            static member TrySideroomDepth = function
                | Hallway _ -> None
                | Sideroom (_, depth) -> Some depth
            static member TryConnectToSideroom = function
                | Hallway x when x % 2 = 0 -> Amphipod.All |> Array.tryItem (x / 2 - 1) |> Option.map (fun amphipod -> (Hallway x, Sideroom (amphipod, 0)))
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

        let subrooms room = [| 0 .. RoomSize - 1 |] |> Array.map(BurrowPosition.ToSideroom room)
        let allPositions = Amphipod.All |> Array.collect subrooms |> Array.append BurrowPosition.AllHallways

        let PositionToIndex x = allPositions |> Array.findIndex ((=)x)
        let IndexToPosition x = allPositions |> Array.item x

        let PositionsToIndex (u, v) = (PositionToIndex u, PositionToIndex v)
        
        let Matrix =  
            let hallways = BurrowPosition.AllHallways |> Array.pairwise
            let connections = BurrowPosition.AllHallways |> Array.choose(BurrowPosition.TryConnectToSideroom)
            let siderooms = Amphipod.All |> Array.collect (subrooms >> Array.pairwise)
            hallways |> Array.append connections |> Array.append siderooms |> Array.map PositionsToIndex |> AdjacencyMatrix

        member x.GetFullPath = PositionsToIndex >> Matrix.GetFullPath >> List.map IndexToPosition
        member x.GetDistance = PositionsToIndex >> Matrix.GetDistance
        member x.GetFullPathFrom u v = x.GetFullPath (u, v)    

    let Solve initialPositions =

        let maxRoomDepth = initialPositions |> List.choose(AmphipodPosition.GetPosition >> BurrowPosition.TrySideroomDepth) |> List.max
        let roomSize = maxRoomDepth + 1

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

            let goHallwayWhileCan x = map.GetFullPathFrom x >> List.takeWhile (occupied >> not) >> List.filter (BurrowPosition.IsInHallway) >> List.filter (BurrowPosition.TryConnectToSideroom >> Option.isNone)
            let possibleMoves = function
                | { Amphipod = _; Position = Sideroom (x, _)  } when cleanRoom x -> []
                | { Amphipod = amphipod; Position = Hallway _ } when not (cleanRoom amphipod) -> []
                | { Amphipod = amphipod; Position = Hallway x } ->
                    [roomSize - 1 .. -1 .. 0]
                    |> List.map (BurrowPosition.ToSideroom amphipod)
                    |> List.tryFind (map.GetFullPathFrom (Hallway x) >> List.forall(occupied >> not))
                    |> Option.map List.singleton
                    |> Option.defaultValue []
                | { Amphipod = _; Position = position } ->
                    goHallwayWhileCan position (Array.head BurrowPosition.AllHallways) @
                    goHallwayWhileCan position (Array.last BurrowPosition.AllHallways)
        
            let makeMoves x =
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
                | p -> p |> List.collect makeMoves |> List.sort |> List.tryHead |> Memoize p

            evaluateWith positions

        OrderAmphipods initialPositions

    let Parse (arr : string[]) =    
        let parseChar (r, c) = Amphipod.TryParse >> Option.map(fun amphipod -> AmphipodPosition.WithPosition (Sideroom (Amphipod.All[(c - 3) / 2], r - 2)) amphipod)
        let parseRow (row, line : string) = line.ToCharArray() |> Array.indexed |> Array.choose(fun (col, ch) -> parseChar (row, col) ch)

        arr |> Array.indexed |> Array.collect parseRow |> Array.toList

    let data =
        "Input.txt"
        |> System.IO.File.ReadAllLines

    module Task1 =
        let Answer = data |> Parse |> Solve

    module Task2 =
        let toAdd = [|
            "  #D#C#B#A#"
            "  #D#B#A#C#" 
        |]
        let Answer = data |> Array.insertManyAt 3 toAdd |> Parse |> Solve
