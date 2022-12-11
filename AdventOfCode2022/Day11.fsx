#load "Scripts\\ParserLibrary.fsx"
#load "Scripts\\Utils.fsx"
open Utils
open ParserLibrary

module Day11 =

    type Monkey =
        {
            Operation : int64 -> int64
            DivisibilityTest : int64
            DivisibilityOutcome: int * int
            Holdings : int64 list
            ItemsInspected : int
        }
        with
            static member ClearHoldings monkey = { monkey with Holdings = [] }
            static member AddHoldings holdings monkey = { monkey with Holdings = monkey.Holdings @ holdings}
            static member AddInspections items monkey = { monkey with ItemsInspected = monkey.ItemsInspected + items }
            static member TotalItemsInspected monkey = monkey.ItemsInspected
            static member GetDivisibilityTest monkey = monkey.DivisibilityTest

    type OperationArgument =
        | Self
        | Literal of int64
        with
            static member Argument self = function
                | Self -> self
                | Literal x -> x
            static member Parser = choice [
                pstring "old" |>> fun _ -> Self
                pint64 |>> Literal
            ]

    type Operation = Add | Multiply
        with
            static member Parse = function | '+' -> Add | '*' -> Multiply
            static member Function = function | Add -> (+) | Multiply -> (*)
            static member Parser = (pchar '+' <|> pchar '*') |>> Operation.Parse

    let ParseMonkey (lst : string list) =
        let ParseHoldings = many whitespaceChar >>. pstring "Starting items: " >>. sepBy pint64 (pstring ", ")
        let ParseDivisibilityTest = many whitespaceChar >>. pstring "Test: divisible by " >>. pint64
        let ParseMonkeyToThrowTo =
            many whitespaceChar
            >>. pstring "If "
            >>. choice [pstring "true"; pstring "false"]
            >>. pstring ": throw to monkey "
            >>. pint
        let ParseOperation =
            many whitespaceChar
            >>. pstring "Operation: new = "
            >>. OperationArgument.Parser
            .>>. between whitespaceChar Operation.Parser whitespaceChar
            .>>. OperationArgument.Parser
            |>> fun ((argument1, operation), argument2) ->
                fun x ->
                    (Operation.Function operation) (OperationArgument.Argument x argument1) (OperationArgument.Argument x argument2)

        {
            Holdings = ParseLine ParseHoldings (lst[1])
            Operation = ParseLine ParseOperation (lst[2])
            DivisibilityTest = ParseLine ParseDivisibilityTest (lst[3])
            DivisibilityOutcome = (ParseLine ParseMonkeyToThrowTo (lst[4]), ParseLine ParseMonkeyToThrowTo (lst[5]))
            ItemsInspected = 0
        }

    let rec ParseInput = function
        | [] -> []
        | ""::lst -> ParseInput lst
        | l1::l2::l3::l4::l5::l6::tail -> ParseMonkey [l1;l2;l3;l4;l5;l6] :: (ParseInput tail)
        | _ -> failwithf "Bad input"

    let Monkeys = Input() |> Array.toList |> ParseInput

    let Solve worryDivisor roundCount =

        let Monkeys' = Monkeys |> List.toArray // Copy monkeys to array, so it can be modified inplace
        let globalModulo = Monkeys |> List.map(Monkey.GetDivisibilityTest) |> List.reduce (*)

        let runRound roundNumber =

            let InspectOneHolding divisor operation (appendIfTrue, appendIfFalse) currentHolding =
                let newWorryLevel = (operation(currentHolding) / worryDivisor) % globalModulo
                match (newWorryLevel % divisor) with
                    | 0L -> (appendIfTrue @ [newWorryLevel], appendIfFalse)
                    | _ ->  (appendIfTrue, appendIfFalse @ [newWorryLevel])

            let AddHoldigs monkeyNumber additionalHoldings =
                Array.get Monkeys' monkeyNumber |> Monkey.AddHoldings additionalHoldings |> Array.set Monkeys' monkeyNumber

            let Inspect monkeyNumber monkey =
                let (holdings1, holdings2) = monkey.Holdings |> List.fold(InspectOneHolding monkey.DivisibilityTest monkey.Operation) ([], [])
                let (monkey1, monkey2) = monkey.DivisibilityOutcome

                AddHoldigs monkey1 holdings1
                AddHoldigs monkey2 holdings2

                monkey
                |> Monkey.AddInspections (List.length monkey.Holdings) 
                |> Monkey.ClearHoldings
                |> Array.set Monkeys' monkeyNumber
            
            Array.iteri Inspect Monkeys'

        do [1 .. roundCount] |> List.iter runRound

        let sortedInspections = Monkeys' |> Array.map(Monkey.TotalItemsInspected) |> Array.sortDescending
        (int64)sortedInspections[0] * (int64)sortedInspections[1]
    
    module Task1 =
        let Answer = Solve 3L 20

    module Task2 =
        let Answer = Solve 1L 10000

