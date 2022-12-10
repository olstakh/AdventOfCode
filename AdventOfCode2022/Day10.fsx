#load "Scripts\\ParserLibrary.fsx"
#load "Scripts\\Utils.fsx"
open Utils
open ParserLibrary

module Day10 =
    type Instruction =
        | Noop
        | Add of int
        with
            static member Parse (s:string) =
                let tokenized = s.Split(' ')
                if (tokenized[0] = "noop") then Noop else Add (int tokenized[1])

    let data = Input() |> Array.map Instruction.Parse

    module Task1 =
        let tryAdd x = function
            | cycle when cycle % 40 = 20  -> cycle * x
            | _ -> 0

        let rec Simulate (currentCycle, accumulatedSum, registerValue) = function
            | Noop -> (currentCycle + 1, accumulatedSum + tryAdd registerValue currentCycle, registerValue)
            | Add x  -> (currentCycle + 2, accumulatedSum + tryAdd registerValue currentCycle + tryAdd registerValue (currentCycle + 1), registerValue + x)

        let (_, Answer, _) =
            data |> Array.fold Simulate (1, 0, 1)

    module Task2 =
        let addPixel currentCycle currentValue =
            let position = (currentCycle - 1) % 40
            if (position >= currentValue - 1 && position <= currentValue + 1) then "#" else "."

        let rec DrawImage (currentCycle, currentImage, registerValue) = function
            | Noop -> (currentCycle + 1, currentImage + addPixel currentCycle registerValue, registerValue)
            | Add x -> (currentCycle + 2, currentImage + addPixel currentCycle registerValue + addPixel (currentCycle + 1) registerValue, registerValue + x)

        let (_, Answer, _) = 
            data |> Array.fold DrawImage (1, "", 1)

        let rec Draw = function
            | s when String.length s <= 40 -> printfn "%s" s
            | s ->
                do printfn "%s" (s.Substring(0, 40))
                Draw (s.Substring(40))
    
        Draw Answer