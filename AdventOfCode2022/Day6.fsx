#load "Scripts\\Utils.fsx"
open Utils

module Day6 =

    let allDifferent s =
        let len = Seq.length s
        let dlen = Seq.distinct s |> Seq.length
        len = dlen

    let data = Input() |> Array.head

    let Solve windowSize =
        windowSize + (data |> Seq.windowed windowSize |> Seq.findIndex allDifferent)
    
    module Task1 =
        let Answer = Solve 4

    module Task2 =
        let Answer = Solve 14