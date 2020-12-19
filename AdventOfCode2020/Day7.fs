module Day7

open ParserLibrary
open System.Collections.Generic

type ColoredBag = ColoredBag of string

type ContainerBag =
    {
        Bag : ColoredBag
        Contains : (int * ColoredBag) list
    }
    with
        static member GetBag me = me.Bag
        static member Parse (s : string) =
            let bag = pstring "bags" <|> pstring "bag"
            let contain = pstring "contain"
            let comma = pstring ","
            let period = pstring "."
            let regularString = manyChars (anyOf ['a' .. 'z'])
            let regularNumber = manyChars (anyOf ['0' .. '9'])

            let name = regularString .>> spaces .>>. regularString .>> spaces .>> bag |>> fun (first, second) -> first + " " + second
            let nobags = pstring "no" .>> spaces >>. pstring "other" .>> spaces >>. bag
            let somebag = regularNumber .>> spaces .>>. name
            let somebags = sepBy somebag (comma .>> spaces)

            let bagcontains = (nobags |>> fun _ -> []) <|> somebags

            let fullParser = name .>> spaces .>> contain .>> spaces .>>. bagcontains .>> period
                             |>> fun (name, contains) ->
                                {
                                    Bag = ColoredBag name
                                    Contains = contains |> List.map(fun (amount, insideBag) -> int amount, ColoredBag insideBag)
                                }

            match (run fullParser s) with
                | Success (container, _) -> container
                | Failure (_, error, position) -> failwithf "Can't parse the string %s. Error %s. Position: %d" s error position.column

let mutable canFindSaved = Dictionary<ColoredBag, bool>()
let rec CanFind all expected (current : ColoredBag) =
    if (canFindSaved.ContainsKey(current) = false) then
        let res =
            match (all |> Array.tryFind (fun me -> me.Bag = current)) with
            | Some me when me.Bag = expected -> true
            | Some me -> me.Contains |> List.exists(fun (_, insideBag) -> CanFind all expected insideBag)
            | None -> false
        canFindSaved.Add(current, res)

    canFindSaved.[current]

let mutable calculateInsideSaved = Dictionary<ColoredBag, int>()
let rec CalculateInside all (current : ColoredBag) =
    if (calculateInsideSaved.ContainsKey(current) = false) then
        let res =
            match (all |> Array.tryFind(fun me -> me.Bag = current)) with
            | Some me -> me.Contains |> List.sumBy(fun (amount, insideBag) -> amount * (1 + CalculateInside all insideBag))
            | None -> 0
        calculateInsideSaved.Add(current, res)

    calculateInsideSaved.[current]

let Solve1 (fileContent : string[]) =
    let bags = fileContent |> Array.map (ContainerBag.Parse)
    let expectedBag = ColoredBag "shiny gold"

    bags
    |> Array.filter(fun bag -> bag.Bag <> expectedBag)
    |> Array.filter (ContainerBag.GetBag >> (CanFind bags expectedBag))
    |> Array.length

let Solve2 (fileContent : string[]) =
    let bags = fileContent |> Array.map (ContainerBag.Parse)
    let expectedBag = ColoredBag "shiny gold"

    CalculateInside bags expectedBag
