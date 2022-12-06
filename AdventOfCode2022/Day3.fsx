module Day3 =

    let IntersectMany : string[] -> char =
        Array.map(Set.ofSeq) >> Set.intersectMany >> Set.toList >> List.exactlyOne

    let Priority = function
        | lower when System.Char.IsLower(lower) -> int lower - int 'a' + 1
        | upper when System.Char.IsUpper(upper) -> int upper - int 'A' + 27
        | bad -> failwithf "Unknown symbol %c" bad

    let data = 
        "Input.txt"
        |> System.IO.File.ReadAllLines

    let Solve = Seq.map (IntersectMany >> Priority) >> Seq.sum

    module Task1 =
        let Compartmentalize (s:string) =
            let len = s.Length
            [| s.Substring(0, len / 2); s.Substring(len / 2) |]

        let Answer = data |> Array.map Compartmentalize |> Solve

    module Task2 =
        let Answer = data |> Array.chunkBySize 3 |> Solve