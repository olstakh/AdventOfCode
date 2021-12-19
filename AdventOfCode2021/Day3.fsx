module Day3 =

    let input =
        "Input.txt"
        |> System.IO.File.ReadAllLines

    let width = (Array.get input 0).Length
    let initialCount = Array.create width (0, 0)
    let countBits = Array.fold (fun acc x -> Array.zip acc (x.ToString().ToCharArray())
                                             |> Array.map(fun ((zeroes, ones), ch) ->
                                                            if ch = '0' then (zeroes + 1, ones)
                                                                        else (zeroes, ones + 1)
                                        )
                      ) initialCount
    let totalCount = input |> countBits

    let mostCommonBit (zeroes, ones) = if zeroes > ones then '0' else '1'
    let leastCommonBit (zeroes, ones) = if zeroes <= ones then '0' else '1'

    module Task1 =
        let gammaRate =
            totalCount
            |> Array.map mostCommonBit
            |> System.String.Concat

        let epsilonRate =
            totalCount
            |> Array.map leastCommonBit
            |> System.String.Concat                 
    
        let Answer =
            System.Convert.ToInt64(gammaRate, 2)
            *
            System.Convert.ToInt64(epsilonRate, 2)

    module Task2 =
        let rec FindNumberByBitCriteria bitCriteria bitPosition = function
            | [||] -> failwith "Wrong logic!"
            | [|answer|] -> answer
            | numbers ->
                let currentBitCount = Array.get (countBits numbers) bitPosition
                let currentBit = bitCriteria currentBitCount
                numbers
                |> Array.where (fun s -> s.ToString().[bitPosition] = currentBit)
                |> FindNumberByBitCriteria bitCriteria (bitPosition + 1)
                         
        let oxygenGenerator = FindNumberByBitCriteria mostCommonBit 0 input
        let co2Scrubber = FindNumberByBitCriteria leastCommonBit 0 input

        let Answer =
            System.Convert.ToInt64(oxygenGenerator, 2)
            *
            System.Convert.ToInt64(co2Scrubber, 2)