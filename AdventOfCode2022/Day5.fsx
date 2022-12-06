#load "Scripts\\ParserLibrary.fsx"
open ParserLibrary

module Day5 =

    type Crate = Crate of char
        with
            static member Parser =
                pchar '[' >>. anyOf ['A'..'Z'] .>> pchar ']'
                |>> Crate

    let Parser = 
        let p1 = Crate.Parser |>> Some
        let p2 = parseByCount whitespaceChar 3 |>> fun _ -> None
        many ((p1 <|> p2) .>> opt whitespaceChar) |>> List.toArray

    let Parse (s:string) =
        match (run Parser s) with
            | Success (segmentPair, remainingInput) -> segmentPair
            | Failure (label, error, pos) -> failwithf "Failed to parse input. Label: %s, Error : %s, position : %d" label error pos.column

    let allData =
        "Input.txt"
        |> System.IO.File.ReadAllLines

    let emptyLine = allData |> Array.findIndex(fun s -> s.Length = 0)

    let (input', instructions') = Array.splitAt emptyLine allData
    let input = input' |> Array.rev |> Array.tail |> Array.rev // get rid of last string
    let instructions = instructions' |> Array.tail // get rid of first string

    let parsedInput = input |> Array.map Parse |> Array.transpose
