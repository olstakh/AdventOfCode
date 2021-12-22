module Day10 =

    type LineState =
        | Incomplete of char List
        | Corrupted of char
        with
            static member CorruptedValue = function | Corrupted x -> Some x | _ -> None
            static member IncompleteValue = function | Incomplete x -> Some x | _ -> None

    let isOpenChar =  function | '<' | '(' | '[' | '{' -> true | _ -> false
    let isCloseChar = function | '>' | ')' | ']' | '}' -> true | _ -> false
    let doMatch = function | ('<', '>') | ('(', ')') | ('{', '}') | ('[', ']') -> true | _ -> false

    let AnalyzeState (s:string) =
        let rec tryGetScore openChars pos =
            match (pos) with
                | theEnd when theEnd >= s.Length -> openChars |> List.map(fun idx -> s.[idx]) |> Incomplete
                | openChar when s.[openChar] |> isOpenChar -> tryGetScore (openChar::openChars) (pos + 1)
                | closeChar when openChars = [] -> failwith "Bad input, you lied to me!"
                | closeChar when doMatch (s.[List.head openChars], s.[closeChar]) -> tryGetScore (List.tail openChars) (pos + 1)
                | closeChar -> s.[closeChar] |> Corrupted

        tryGetScore [] 0

    let input = 
        "Input.txt"
        |> System.IO.File.ReadAllLines

    module Task1 = 
        let charValue = function | ')' -> 3 | ']' -> 57 | '}' -> 1197 | '>' -> 25137 | _ -> 0

        let Answer =
            input
            |> Array.map AnalyzeState
            |> Array.choose LineState.CorruptedValue
            |> Array.sumBy charValue

    module Task2 =
        let addScore currentScore toAdd = currentScore * 5L + toAdd
        let charValue = function | ')' -> 1L | ']' -> 2L | '}' -> 3L | '>' -> 4L | _ -> 0L
        let charFlip = function | '(' -> ')' | '[' -> ']' | '<' -> '>' | '{' -> '}' | x -> x
        let charScore = charFlip >> charValue

        let results =
            input
            |> Array.map AnalyzeState
            |> Array.choose LineState.IncompleteValue
            |> Array.map(List.map charScore >> List.fold addScore 0L)
            |> Array.sort
            |> Array.distinct

        let Answer = results.[results.Length / 2]