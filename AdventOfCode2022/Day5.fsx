#load "Scripts\\ParserLibrary.fsx"
#load "Scripts\\Utils.fsx"
open Utils
open ParserLibrary

module Day5 =

    type Crate = Crate of char
        with
            static member Parser =
                pchar '[' >>. anyOf ['A'..'Z'] .>> pchar ']'
                |>> Crate
            static member ToChar = function
                | Crate ch -> ch

    type Move =
        {
            CraneCount : int
            FromStack : int
            ToStack : int
        } with
            static member Parser =
                pstring "move"  .>> whitespaceChar  >>. pint .>> whitespaceChar .>>
                pstring "from"  .>> whitespaceChar .>>. pint .>> whitespaceChar .>>
                pstring "to"    .>> whitespaceChar .>>. pint
                |>> fun ((craneCount, fromStack), toStack) ->
                    { CraneCount = craneCount; FromStack = fromStack; ToStack = toStack }

            static member ApplyMove preserveOrder cranes move =
                let fromStack = Array.get cranes (move.FromStack - 1)
                let cranesToMove = List.take (move.CraneCount) fromStack
                let toStack = Array.get cranes (move.ToStack - 1)

                let cranesToMove' =
                    match preserveOrder with
                    | true -> cranesToMove
                    | false -> List.rev cranesToMove

                let fromStack' = List.skip (move.CraneCount) fromStack
                let toStack' = cranesToMove' @ toStack

                Array.set cranes (move.FromStack - 1) fromStack'
                Array.set cranes (move.ToStack - 1) toStack'

                cranes

    // Ex: list of "[x]" or "   ", followed by a space
    let LineParser = 
        let p1 = Crate.Parser |>> Some
        let p2 = parseByCount whitespaceChar 3 |>> fun _ -> None
        many ((p1 <|> p2) .>> opt whitespaceChar) |>> List.toArray

    let ReadInput() =
        let allData = Input()

        let emptyLine = allData |> Array.findIndex(fun s -> s.Length = 0)

        let (input', instructions') = Array.splitAt emptyLine allData
        let input = input' |> Array.rev |> Array.tail |> Array.rev // get rid of last string
        let instructions = instructions' |> Array.tail // get rid of first string
        (input, instructions)

    let (input, instructions) = ReadInput()

    let Solve preserveOrder =

        let parsedInput = input |> Array.map (ParseLine LineParser) |> Array.transpose |> Array.map (Array.toList >> List.choose id)
        let parsedInstructions = instructions |> Array.map (ParseLine (Move.Parser))

        parsedInstructions
        |> Array.fold (Move.ApplyMove preserveOrder) parsedInput
        |> Array.map (List.head >> Crate.ToChar)
        |> System.String.Concat

    module Task1 =
        let Answer = Solve false

    module Task2 =
        let Answer = Solve true