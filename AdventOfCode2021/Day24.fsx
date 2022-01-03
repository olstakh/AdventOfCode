module Day24 =

    let nextZ currentZ (z,y,x) d =
        let z' = currentZ / z
        if (currentZ % 26L + x = d) then z' else z' * 26L + d + y    

    let rec TryFind digitsOrder (zDivizors, yAdditions, xAdditions) = function
        | 0L when List.isEmpty zDivizors -> Some ""
        | _ when List.isEmpty zDivizors -> None
        | currentZ when currentZ - 100L > (zDivizors |> List.fold (*) 1L) -> None
        | currentZ ->   let (z::zTail, y::yTail, x::xTail) = (zDivizors, yAdditions, xAdditions)
                        digitsOrder
                        |> List.tryPick(fun d ->
                            int64 d
                            |> nextZ currentZ (z, y, x)
                            |> TryFind digitsOrder (zTail, yTail, xTail)
                            |> Option.map(fun res -> string d + res)
                        )    

    let Solve digitsOrder =
        let parse (z, y, x) (s:string) =
            match (s.Split(' ')) with
            | [| "div"; "z"; number |] -> (z @ [int64(number)], y, x)
            | [| "add"; "y"; number |] when number <> "1" && number <> "25" && number <> "w" -> (z, y @ [int64(number)], x)
            | [| "add"; "x"; number |] when number <> "z" -> (z, y, x @ [int64(number)])
            | _ -> (z, y, x)

        let (z, y, x) = "Input.txt" |> System.IO.File.ReadAllLines |> Array.fold parse ([],[],[])
        TryFind digitsOrder (z, y, x) 0L

    module Task1 = 
        let Answer = Solve [9 .. -1 .. 1]

    module Task2 =
        let Answer = Solve [1 .. +1 .. 9]
