module Day8 =

    (*
     0000
    1    2
    1    2
     3333       <- Display, with bits showing which segments they correspond to
    4    5
    4    5
     6666
    *)

    let DisplayDigitsMasks = [|
    // 0: 012_456
        0b1110111;

    // 1: __2__5_
        0b0010010

    // 2: 0_234_6
        0b1011101

    // 3: 0_23_56
        0b1011011

    // 4: _123_5_ 
        0b0111010

    // 5: 01_3_45
        0b1101011

    // 6: 01_3456 
        0b1101111

    // 7: 0_2__5_
        0b1010010

    // 8: 0123456
        0b1111111

    // 9: 0123_56
        0b1111011
    |]

    let isKnownMask m = DisplayDigitsMasks |> Array.exists(fun knownMask -> knownMask = m)
    let getDigit m = DisplayDigitsMasks |> Array.findIndex(fun knownMask -> knownMask = m) |> fun ind -> char(ind + int '0')

    let power2 = [|0 .. 6|] |> Array.map(fun p2 -> 1 <<< p2)

    let AllPermutations list =
        let rec inserts e = function
            | [] -> [[e]]
            | x::xs as list -> (e::list)::[for xs' in inserts e xs -> x::xs']

        List.fold (fun accum x -> List.collect (inserts x) accum) [[]] list    

    let Solve (inputData : string, outputData : string) =
        let inputNumbers = inputData.Split(' ')
        let outputNumbers = outputData.Split(' ')

        let tryApplyMapping (mapping : int[]) =
            let getMask (s:string) =
                s.ToCharArray()
                |> Array.map(fun ch -> power2.[mapping.[int ch - int 'a']])
                |> Array.fold (|||) 0

            inputNumbers
            |> Array.forall (getMask >> isKnownMask)
            |> Some
            |> Option.filter(fun x -> x)
            |> Option.map(fun _ -> outputNumbers |> Array.map (getMask >> getDigit) |> System.String.Concat)

        [0 .. 6]
        |> AllPermutations
        |> List.map(List.toArray)
        |> List.choose tryApplyMapping
        |> List.exactlyOne

    let input =
        "Input.txt"
        |> System.IO.File.ReadAllLines

    let data = input |> Array.map(fun line -> let [| inputData; outputData |] = line.Split(" | ") in (inputData, outputData))

    module Task1 =
        let Answer =
            data
            |> Array.sumBy(Solve >> String.filter(fun ch -> ch = '1' || ch = '4' || ch = '7' || ch = '8') >> String.length)
        
    module Task2 =
        let Answer =
            data
            |> Array.sumBy(Solve >> int)