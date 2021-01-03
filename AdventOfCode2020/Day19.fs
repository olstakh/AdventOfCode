module Day19

open ParserLibrary
open Common

type RuleId = RuleId of int with
    static member Parser = pint .>> spaces |>> RuleId

type RuleCombination = RuleCombination of RuleId list with
    static member Parser =
        spaces >>. many RuleId.Parser |>> RuleCombination

type Rule =
    | Direct of char
    | Indirect of RuleCombination list
    with
        static member Parser =
            let pDirect =   betweenSame (pchar '"') (anyOf ['a' .. 'z'])
                            |>> Direct
            let pIndirect = sepBy (RuleCombination.Parser) (pchar '|')
                            |>> Indirect

            pDirect <|> pIndirect

let ParseRule allRules s =
    let ruleParser = RuleId.Parser .>> pchar ':' .>> spaces .>>. Rule.Parser
    match (run ruleParser s) with
    | Success((ruleId, parsedRule), _) -> allRules |> Map.add ruleId parsedRule
    | Failure (_, error, position) -> failwithf "Can't parse the string %s. Error %s. Position: %d" s error position.column

let SplitRulesAndInput (fileContent : string[]) =
    let [input; rules] =
        fileContent |> Array.fold (addToLastOrAppend (fun v lst -> lst@[v]) []) [[]]
    (rules |> List.fold ParseRule Map.empty, input)

let MatchesFirstRule allRules =
    let rec tryMatchRuleList s (RuleCombination rules) =
        rules |> List.fold(fun acc rule -> acc |> List.collect (tryMatchRule rule)) [s]

    and tryMatchRule ruleId s =
        match (allRules |> Map.find ruleId) with
        | _ when (String.length s = 0) -> []
        | Direct ch when s.[0] = ch -> [s.Substring(1)]
        | Direct _ -> []
        | Indirect choiceList -> choiceList |> List.collect (tryMatchRuleList s)

    tryMatchRule (RuleId 0) >> List.contains ""

let Solve1 (fileContent : string[]) =
    let (rules, input) = SplitRulesAndInput fileContent

    input
    |> List.filter (MatchesFirstRule rules)
    |> List.length

let Solve2 (fileContent : string[]) =
    
    let substitution = function
        | "8: 42" -> "8: 42 | 42 8"
        | "11: 42 31" -> "11: 42 31 | 42 11 31"
        | s -> s

    let (rules, input) = SplitRulesAndInput (fileContent |> Array.map substitution)

    input
    |> List.filter (MatchesFirstRule rules)
    |> List.length
    
