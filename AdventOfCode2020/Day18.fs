module Day18

open ParserLibrary

type Operation =
    | Add
    | Multiply
    with
        static member Parse = function | '+' -> Add | '*' -> Multiply
        static member Apply = function | Add -> (+) | Multiply -> (*)

type Expression =
    | Number of int64
    | SimpleExpression of Expression * (Operation * Expression) list
    | NestedExpression of Expression

let ParseExpressionWith simpleExpressionParser s =
    // Need a placeholder, since expression's definition is recursive
    let pExpr, pExprRef = createParserForwardedToRef<Expression>()

    let pNumber = pint64 .>> spaces |>> Number

    let pAddition = pchar ('+') .>> spaces |>> Operation.Parse
    let pMultiplication = pchar ('*') .>> spaces |>> Operation.Parse

    let pNestedExpression =
        let left = pchar '(' .>> spaces
        let right = pchar ')' .>> spaces

        between left pExpr right |>> NestedExpression

    let pSubExpression = pNestedExpression <|> pNumber

    pExprRef := simpleExpressionParser pSubExpression pAddition pMultiplication

    match (run pExpr s) with
    | Success(parsed, _) -> parsed
    | Failure (_, error, position) -> failwithf "Can't parse the string %s. Error %s. Position: %d" s error position.column

let rec CalculateExpression = function
    | Number x -> x
    | NestedExpression ex -> CalculateExpression ex
    | SimpleExpression (firstTerm, lst) ->
        lst
        |> List.fold(
            fun left (op, right) ->
                Operation.Apply op left (CalculateExpression right)
        ) (CalculateExpression firstTerm)

let SolveWith expressionParser =
    Array.Parallel.map(ParseExpressionWith expressionParser >> CalculateExpression)
    >> Array.sum

let Solve1 (fileContent : string[]) =

    let simpleExpressionParser pSubExpression pAddition pMultiplication =
        pSubExpression .>>. many ((pAddition <|> pMultiplication) .>>. pSubExpression .>> spaces)
        |>> SimpleExpression

    SolveWith simpleExpressionParser fileContent

let Solve2 (fileContent : string[]) =
    
    let simpleExpressionParser pSubExpression pAddition pMultiplication =
        let pFactors =
            pSubExpression .>>. many (pAddition .>>. pSubExpression .>> spaces)
            |>> SimpleExpression

        // We multiplying factors, instead of adding terms, because + has more priority than *
        pFactors .>>. many (pMultiplication .>>. pFactors .>> spaces)
        |>> SimpleExpression

    SolveWith simpleExpressionParser fileContent