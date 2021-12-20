module Day6 =

    let Solve daysAhead lst =
        let DP = Array2D.create 500 10 -1L

        let rec CalcFish remainingDays = function
            | remainingCycle when DP.[remainingDays, remainingCycle] <> -1L -> DP.[remainingDays, remainingCycle]
            | remainingCycle when remainingCycle >= remainingDays -> 0L
            | remainingCycle -> let res =
                                    (CalcFish (remainingDays - remainingCycle - 1) 6) +
                                    (CalcFish (remainingDays - remainingCycle - 1) 8) + 1L
                                do DP.[remainingDays, remainingCycle] <- res
                                res

        lst
        |> List.sumBy (CalcFish daysAhead >> (+)1L)

    let input =
        "Input.txt"
        |> System.IO.File.ReadAllText

    let daysLeft = input.Split(',') |> Array.map int |> Array.toList

    module Task1 =
        let Answer = Solve 80 daysLeft

    module Task2 =
        let Answer = Solve 256 daysLeft
