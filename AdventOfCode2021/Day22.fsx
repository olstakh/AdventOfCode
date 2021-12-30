#load "Scripts\\ParserLibrary.fsx"
open ParserLibrary

module Day22 =
    type Cube =
        {
            minX : int; minY : int; minZ : int
            maxX : int; maxY : int; maxZ : int
            Value : int
        }
        with
            static member Volume cube = int64(cube.maxX - cube.minX) * int64(cube.maxY - cube.minY) * int64(cube.maxZ - cube.minZ)
            static member OutsideOF cube cube' =
                cube'.minX >= cube.maxX || cube'.maxX <= cube.minX ||
                cube'.minY >= cube.maxY || cube'.maxY <= cube.minY ||
                cube'.minZ >= cube.maxZ || cube'.maxZ <= cube.minZ
            static member IntersectWith cube = function
                | x when x |> Cube.OutsideOF cube -> [x]
                | x when x.minX < cube.minX -> { x with maxX = cube.minX } :: (Cube.IntersectWith cube { x with minX = cube.minX })
                | x when x.maxX > cube.maxX -> { x with minX = cube.maxX } :: (Cube.IntersectWith cube { x with maxX = cube.maxX })
                | x when x.minY < cube.minY -> { x with maxY = cube.minY } :: (Cube.IntersectWith cube { x with minY = cube.minY })
                | x when x.maxY > cube.maxY -> { x with minY = cube.maxY } :: (Cube.IntersectWith cube { x with maxY = cube.maxY })
                | x when x.minZ < cube.minZ -> { x with maxZ = cube.minZ } :: (Cube.IntersectWith cube { x with minZ = cube.minZ })
                | x when x.maxZ > cube.maxZ -> { x with minZ = cube.maxZ } :: (Cube.IntersectWith cube { x with maxZ = cube.maxZ })
                | _ -> []
            static member Worth cube = int64(cube.Value) * Cube.Volume cube

    let Parse (s:string) =
        let pCubeValue = pstring "on" <|> pstring "off" |>> function | "on" -> 1 | _ -> 0
        let pCoordinates ch = pchar ch .>> pchar '=' >>. pint .>> pstring ".." .>>. pint
        let pCube = pCubeValue .>> spaces .>>. (pCoordinates 'x') .>> pchar ',' .>>. (pCoordinates 'y') .>> pchar ',' .>>. (pCoordinates 'z')
                    |>> (fun (((value, (minX, maxX)), (minY, maxY)), (minZ, maxZ)) ->
                            {
                                minX = minX; minY = minY; minZ = minZ;
                                maxX = maxX + 1; maxY = maxY + 1; maxZ = maxZ + 1;
                                Value = value 
                            })
        match (run pCube s) with
            | Success (cube, _) -> cube
            | Failure (label, error, pos) -> failwithf "Failed to parse input. Label: %s, Error : %s, position : %d" label error pos.column

    let Solve =
        Array.fold (fun cubes newCube -> cubes |> List.collect (Cube.IntersectWith newCube) |> List.append [newCube]) []
        >> List.sumBy Cube.Worth

    let allCubes =
        "Input.txt"
        |> System.IO.File.ReadAllLines
        |> Array.map Parse

    module Task1 =
        let Answer = Solve (allCubes |> Array.filter(fun cube -> cube.maxX <= 50 && cube.maxY <= 50 && cube.maxZ <= 50 && cube.minX >= -50 && cube.minY >= -50 && cube.maxZ >= -50))

    module Task2 =
        let Answer = Solve allCubes
        