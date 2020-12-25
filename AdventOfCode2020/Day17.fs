module Day17

open Common

type CubeState = Active | Inactive
    with
        static member Parse = function
            | '#' -> Active | '.' -> Inactive
            | c -> failwithf "Unknown cube state %c" c
        static member IsActive' = function | Active -> true | _ -> false

let cubeState allCubes = function
    | cube when Set.contains cube allCubes -> Active
    | _ -> Inactive
    
let changeCubeStateRule activeNeighbours = function
    | Active when List.length activeNeighbours = 2 || List.length activeNeighbours = 3 -> Active
    | Active -> Inactive
    | Inactive when List.length activeNeighbours = 3 -> Active
    | Inactive -> Inactive

let minMaxBy f set =
    let flat = set |> Set.toList |> List.map f
    let rangeStart = flat |> List.min
    let rangeEnd = flat |> List.max
    [rangeStart - 1 .. rangeEnd + 1]
    
let run turns spaceToScanFunc neighbours initialActiveCubes =

    let recalculateCubeState allCubes cube =
        let currentCubeState = cubeState allCubes cube
        let activeNeighbours = neighbours cube |> List.filter(cubeState allCubes >> CubeState.IsActive')
        changeCubeStateRule activeNeighbours currentCubeState
    
    let runTurn activeCubes =
        spaceToScanFunc activeCubes
        |> List.filter(recalculateCubeState activeCubes >> CubeState.IsActive')
        |> Set.ofList

    GenerateSome runTurn initialActiveCubes
    |> Seq.take (turns + 1)
    |> Seq.last

let ParseCubes generateInitialCube fileContent =
    fileContent
    |> Array.mapi(
        fun y row ->
            row |> Seq.mapi(fun x ch -> (generateInitialCube x y, CubeState.Parse ch))
                |> Seq.choose(fun (cube, state) -> if state = Active then Some cube else None)
                |> Set.ofSeq
    )
    |> Array.fold Set.union Set.empty
      
let Solve1 (fileContent : string[]) =

    let generateCoords3d xLst yLst zLst =
        [
            for x in xLst do
                for y in yLst do
                    for z in zLst do
                        yield { X = x; Y = y; Z = z }
        ]

    let spaceToScan allCubes =
        generateCoords3d
            (minMaxBy Coord3D.GetX allCubes)
            (minMaxBy Coord3D.GetY allCubes)
            (minMaxBy Coord3D.GetZ allCubes)

    let neighboursDeltaCoords =
        generateCoords3d [-1 .. +1] [-1 .. +1] [-1 .. +1]
        |> List.filter((<>)Coord3D.Origin)

    fileContent
    |> ParseCubes (fun x y -> { X = x; Y = y; Z = 1})
    |> run 6 spaceToScan (fun cube -> neighboursDeltaCoords |> List.map((+) cube))
    |> Set.count

let Solve2 (fileContent : string[]) =

    let generateCoords4d xLst yLst zLst wLst =
        [
            for x in xLst do
                for y in yLst do
                    for z in zLst do
                        for w in wLst do
                            yield { X = x; Y = y; Z = z; W = w }
        ]

    let spaceToScan allCubes =
        generateCoords4d
            (minMaxBy Coord4D.GetX allCubes)
            (minMaxBy Coord4D.GetY allCubes)
            (minMaxBy Coord4D.GetZ allCubes)
            (minMaxBy Coord4D.GetW allCubes)

    let neighboursDeltaCoords =
        generateCoords4d [-1 .. +1] [-1 .. +1] [-1 .. +1] [-1 .. +1]
        |> List.filter((<>)Coord4D.Origin)

    fileContent
    |> ParseCubes (fun x y -> { X = x; Y = y; Z = 1; W = 1 })
    |> run 6 spaceToScan (fun cube -> neighboursDeltaCoords |> List.map((+) cube))
    |> Set.count