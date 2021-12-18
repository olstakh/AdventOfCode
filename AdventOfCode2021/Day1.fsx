let Input = 
    "Input.txt"
    |> System.IO.File.ReadAllLines
    |> Array.map int

let Task1 =
    Input
    |> Array.pairwise
    |> Array.filter(fun (a, b) -> a < b)
    |> Array.length

let Task2 =
    Input
    |> Array.windowed 3
    |> Array.map (Array.sum)
    |> Array.pairwise
    |> Array.filter(fun (a, b) -> a < b)
    |> Array.length