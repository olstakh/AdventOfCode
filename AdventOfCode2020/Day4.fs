module Day4

open System

type Passport =
    {
        byr : string
        iyr : string
        eyr : string
        hgt : string
        hcl : string
        ecl : string
        pid : string
        cid : string option
    } with
        static member TryParse (s : string) =
            let parsed =
                s.Split(' ', StringSplitOptions.RemoveEmptyEntries)
                |> Array.map(fun s -> let [| key; value |] = s.Split(':') in (key.ToLowerInvariant(), value))

            let tryGetValue key =
                parsed |> Array.tryFind(fst >> (=)key) |> Option.map snd

            let [byr; iyr; eyr; hgt; hcl; ecl; pid; cid] as values =
                [
                    "byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid"; "cid"
                ]
                |> List.map tryGetValue

            if (values |> List.take 7 |> List.forall(Option.isSome)) then
                {
                    byr = byr.Value; iyr = iyr.Value; eyr = eyr.Value;
                    hgt = hgt.Value; hcl = hcl.Value; ecl = ecl.Value;
                    pid = pid.Value; cid = cid
                }
                |> Some
            else None

let private ParsePassports =
    Array.fold Common.addToLastOrAppend [""]
    >> List.filter(String.IsNullOrEmpty >> not)
    >> List.choose (Passport.TryParse)
    

let Solve1 (fileContent : string[]) =
    fileContent
    |> ParsePassports 
    |> List.length

let Solve2 (fileContent : string[]) =
    
    let validateByr (s : string) =
        match (Int32.TryParse(s)) with
        | (true, v) when 1920 <= v && v <= 2002 -> true
        | _ -> false

    let validateIyr (s : string) =
        match (Int32.TryParse(s)) with
        | (true, v) when 2010 <= v && v <= 2020 -> true
        | _ -> false

    let validateEyr (s : string) =
        match (Int32.TryParse(s)) with
        | (true, v) when 2020 <= v && v <= 2030 -> true
        | _ -> false

    let validateHgt (s : string) =
        let (number, uom) = (s.Substring(0, s.Length - 2), s.Substring(s.Length - 2))
        match (Int32.TryParse(number), uom) with
        | ((true, v), "cm") when 150 <= v && v <= 193 -> true
        | ((true, v), "in") when 59 <= v && v <= 76 -> true
        | _ -> false

    let validateHcl (s : string) = 
        match (s.[0], s.Substring(1)) with
        | ('#', suff) when suff.Length = 6 ->
            suff |> String.forall(fun ch -> '0' <= ch && ch <= '9' || 'a' <= ch && ch <= 'f')
        | _ -> false

    let validateEcl (s : string) =
        s = "amb" || s = "blu" || s = "brn" ||
        s = "gry" || s = "grn" || s = "hzl" ||
        s = "oth"

    let validatePid (s : string) =
        s.Length = 9 &&
        s |> String.forall(fun ch -> '0' <= ch && ch <= '9')

    let validatePassport p =
        validateByr p.byr &&
        validateIyr p.iyr && 
        validateEyr p.eyr && 
        validateHgt p.hgt &&
        validateHcl p.hcl &&
        validateEcl p.ecl &&
        validatePid p.pid
    
    fileContent
    |> ParsePassports
    |> List.filter validatePassport
    |> List.length
