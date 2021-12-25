#load "Scripts\\ParserLibrary.fsx"
open ParserLibrary

module Day18 =
    type PairValue =
        | Literal of int64
        | Nested of PairValue * PairValue
        with
            static member (+) (left, right) = Nested (left, right)
            static member Magnitude = function
                | Literal v -> v
                | Nested (left, right) ->
                    3L * (PairValue.Magnitude left) +
                    2L * (PairValue.Magnitude right)

    type Action =
        | Explode of (int64 * int64)
        | Split                    

    // string to PairValue parser
    let pairValueParser =
        // Parser is self-referencing, need a placeholder reference
        let (pairValueParser, pairValueParserRef) = createParserForwardedToRef<PairValue>()

        let pNested = between (pchar '[') (pairValueParser .>> pchar ',' .>>. pairValueParser) (pchar ']')
        pairValueParserRef.Value <-
            choice [
                pint64 |>> Literal
                pNested |>> Nested
            ]
        pairValueParser

    let parsePairValue (pairValue : string) =
        match (run pairValueParser pairValue) with
            | Success (parsedPair, _) -> parsedPair
            | Failure (label, error, pos) -> failwithf "Failed to parse string %s; Error label: %s, Message: %s, Column: %d" pairValue label error (pos.column)

    // Adds value to the leftmost or rightmost child
    let rec add v applyToLeft = function 
        | root when v = 0L -> root // nothing to add
        | Literal x -> Literal (x + v)
        | Nested (left, right) when applyToLeft -> Nested (add v true left, right )
        | Nested (left, right)                  -> Nested (left, add v false right)

    let tryExplode =
        let rec tryExplodeInternal depth = function
            | Nested (Literal left, Literal right) when depth = 4 -> Some (Explode (left, right), Literal 0)
            | Nested (left, right) as pp -> 
                match (tryExplodeInternal (depth + 1) left) with
                | Some (Explode (addToLeft, addToRight), newLeft) -> Some (Explode (addToLeft, 0), Nested (newLeft, add addToRight true right))
                | _ ->
                    match (tryExplodeInternal (depth + 1) right) with
                    | Some (Explode (addToLeft, addToRight), newRight) -> Some (Explode (0, addToRight), Nested (add addToLeft false left, newRight))
                    | _ -> None
            | _ -> None
        tryExplodeInternal 0

    let rec trySplit = function
        | Literal v when v >= 10 -> Some (Split, Nested (Literal (v / 2L), Literal ((v + 1L) / 2L)))
        | Nested (left, right) ->
            match (trySplit left) with
            | Some (Split, newLeft) -> Some (Split, Nested (newLeft, right))
            | _ ->
                match (trySplit right) with
                | Some (Split, newRight) -> Some (Split, Nested (left, newRight ))
                | _ -> None
        | _ -> None

    let tryPerformAction pairValue =
        [tryExplode; trySplit]
        |> List.tryPick (fun actionFn -> actionFn(pairValue))
        |> Option.map snd

    let Normalize =
        Some
        >> Seq.unfold (Option.map(fun p -> p, tryPerformAction p))
        >> Seq.last

    let AddPairs a b = Normalize (a + b)

    let input =
        "Input.txt"
        |> System.IO.File.ReadAllLines
        |> Array.map parsePairValue

    module Task1 =
        let Answer =
            input
            |> Array.reduce AddPairs
            |> PairValue.Magnitude

    module Task2 =
        let Answer = 
            [
                for p1 in input do
                    for p2 in input do
                        if (p1 <> p2) then yield AddPairs p1 p2
            ]
            |> List.map (PairValue.Magnitude)
            |> List.max