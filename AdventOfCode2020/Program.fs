// Learn more about F# at http://fsharp.org

open System
open System.IO
open Day8

[<EntryPoint>]
let main argv =
    let filePath = @"C:\Users\olstakh\Source\Repos\AdventOfCode\AdventOfCode2020\Input.txt"
    Solve2 (File.ReadAllLines(filePath))
    |> printfn "%d"

    0 // return an integer exit code
