// Learn more about F# at http://fsharp.org

open System.IO

[<EntryPoint>]
let main argv =
    argv
    |> Array.tryExactlyOne
    |> Option.defaultValue("..\..\..\Input.txt")
    |> File.ReadAllLines
    |> Day14.Solve1
    |> printfn "%d"

    0 // return an integer exit code
