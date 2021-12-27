module Day20 =
    let getAllCoordinates ((minR, minC), (maxR, maxC)) = [|
        for r = minR to maxR do
            for c = minC to maxC do
                (r, c)
    |]

    let changeImageWith imageEnhancement (lights, defaultLight) =
        let allRows = lights |> Seq.toList |> List.map fst
        let allCols = lights |> Seq.toList |> List.map snd
        let (minR, maxR) = (List.min allRows, List.max allRows)
        let (minC, maxC) = (List.min allCols, List.max allCols)

        let fromCoord = function
            | (r, c) when r < minR || c < minC || r > maxR || c > maxC -> defaultLight
            | (r, c) when Set.contains (r, c) lights -> 1
            | _ -> 0

        let getSurrounding (r, c) =
            getAllCoordinates ((r - 1, c - 1), (r + 1, c + 1))
            |> Array.map fromCoord
            |> Array.reduce(fun a b -> a * 2 + b)

        let isLight = getSurrounding >> Array.get imageEnhancement >> (=)('#')

        let newLights =
            getAllCoordinates ((minR - 1, minC - 1), (maxR + 1, maxC + 1))
            |> Array.Parallel.choose (Some >> Option.filter isLight)
            |> Set.ofArray

        let newDefaultLight =
            match (defaultLight) with
            | 0 when Array.head imageEnhancement = '#' -> 1
            | 1 when Array.last imageEnhancement = '.' -> 0
            | _ -> defaultLight

        (newLights, newDefaultLight)

    let input =
        "Input.txt"
        |> System.IO.File.ReadAllLines

    let imageEnhancement = input[0].ToCharArray()
    let image = input |> Array.skip 2

    let lights =
        getAllCoordinates ((0, 0), (image.Length - 1, image[0].Length - 1))
        |> Array.filter (fun (r, c) -> image[r][c] = '#')
        |> Set.ofArray

    let Solve changeImageCount =
        changeImageWith imageEnhancement
        |> List.replicate changeImageCount
        |> List.reduce (>>) <| (lights, 0) // default value is light off
        |> fst
        |> Set.count
    
    module Task1 =
        let Answer = Solve 2
    
    module Task2 =
        let Answer = Solve 50