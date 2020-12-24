module Day16

open ParserLibrary
open Common

type Range = Range of int * int
    with
        static member Fits n (Range (start, finish)) = start <= n && n <= finish

type Rule = Rule of string * Range list
    with
        static member Parse s =
            let pName = manyChars(anyOf (['a' .. 'z']@[' ']))
            let pRange = pint .>> pchar '-' .>>. pint |>> Range

            let pLine = pName .>> pstring ": " .>>. sepBy pRange (pstring " or ") |>> Rule

            match (run pLine s) with
                | Success(parsed, _) -> parsed
                | Failure (_, error, position) -> failwithf "Can't parse the string %s. Error %s. Position: %d" s error position.column
        
        static member GetRules (Rule(name, rules)) = rules
        static member GetName (Rule(name, rules)) = name

let fitsAnyRange n = List.exists(Range.Fits n)
let fitsRule n = Rule.GetRules >> fitsAnyRange n

let tryApplyRule n = Some >> Option.filter(fitsRule n)

let fitsAnyRule n = Array.exists(fitsRule n)
let errorStateBy rules =
    Array.filter(fun n -> not (fitsAnyRule n rules))
    >> Some
    >> Option.filter(Array.isEmpty >> not)
    >> Option.map(Array.sum)

let assignPossibleRules rules =
    Array.map(fun n -> rules |> Array.choose(tryApplyRule n))

let parseFile (fileContent : string[]) =
    let splitTicket (s:string) = s.Split(',') |> Array.map int

    let rules =
        fileContent
        |> Array.takeWhile(System.String.IsNullOrEmpty >> not)
        |> Array.map (Rule.Parse)

    let myTicket =
        fileContent
        |> Array.skip (rules.Length + 2)
        |> Array.head
        |> splitTicket

    let nearbyTickets =
        fileContent
        |> Array.skip (rules.Length + 5)
        |> Array.map splitTicket

    (rules, myTicket, nearbyTickets)

let Solve1 (fileContent : string[]) =
    let (rules, myTicket, nearbyTickets) = parseFile fileContent
    nearbyTickets |> Array.sumBy (errorStateBy rules >> Option.defaultValue 0)

let Solve2 (fileContent : string[]) =
    let (rules, myTicket, nearbyTickets) = parseFile fileContent

    let validNearbyTickets = nearbyTickets |> Array.filter(errorStateBy rules >> Option.isNone)

    let columnRanges = validNearbyTickets |> Array.transpose

    let intersectColumnForAllTickets = Seq.map(Set.ofSeq) >> Set.intersectMany
    
    let columnMapping =
        columnRanges
        |> Array.map(assignPossibleRules rules)
        |> Array.indexed
        |> Array.map(applySnd intersectColumnForAllTickets)
        |> Array.sortBy(snd >> Set.count)
        |> Array.fold(
            fun establishedMapping (ind, currentRules) ->
                let newRule =
                    (Set.difference currentRules (Set.map snd establishedMapping))
                    |> Set.toList
                    |> List.exactlyOne

                Set.add (ind, newRule) establishedMapping
            ) Set.empty
        |> Set.toArray
        |> Array.sortBy fst
        |> Array.map (snd >> Rule.GetName)

    let filteredColumns =
        myTicket
        |> Array.zip columnMapping
        |> Array.filter(fun (name, v)-> name.StartsWith("departure"))

    filteredColumns
    |> Array.map (snd >> int64)
    |> Array.fold (*) 1L
