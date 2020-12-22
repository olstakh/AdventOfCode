module Modulo

let isPrime n =
    Seq.unfold(fun t -> Some(t, t + 1)) 2
    |> Seq.takeWhile(fun t -> t * t <= n)
    |> Seq.exists(fun t -> n % t = 0)
    |> not

let ( +% ) (a, b) p = (a + b) % p
let ( *% ) (a, b) p = (a * b) % p
let ( -~% ) a p = ((p - a % p + 0L) % p)
let ( +~% ) a p = ((p + a % p + 0L) % p)
let rec ( ^% ) (a, b) p =
    if (b = 0L) then 1L
    else
        let t = (a, b/2L) ^% p
        let t2 = (t, t) *% p
        if (b % 2L = 1L) then (a, t2) *% p else t2
let ( ^~% ) a p = (a, p - 2L) ^% p
let ( /% ) (a, b) p = (a, b ^~% p) *% p

// Solves the following equation
// X % p[i] = a[i], given as array of tuples (a[i], p[i])
// (assuming every pair of [p] is co-prime and 0 <= a[i] < p[i])
let SolveGarner primeModulesAndRemainders =
    let (remainders, modules) = Array.unzip primeModulesAndRemainders
    let mCount = modules |> Array.length

    let R = Array2D.init mCount mCount
                (fun i j -> modules.[i] ^~% modules.[j])

    remainders
    |> Array.indexed
    |> Array.fold(
        fun xlst (i, remainder) ->
                xlst
                |> List.indexed
                |> List.fold(
                    fun x (j, xprev) ->
                        (R.[j, i] * (x - xprev)) +~% modules.[i]
                ) remainder
                |> List.singleton
                |> List.append xlst
    ) []
    |> List.indexed
    |> List.sumBy (
        fun (ind, x) ->
            modules |> Array.take ind |> Array.fold (*) x
    ) 
