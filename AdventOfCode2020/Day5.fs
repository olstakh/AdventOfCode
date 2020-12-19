module Day5

open System

let ToBinaryWithSubstitution (zeroChar, oneChar) (s : string) =
    Convert.ToInt32(s.Replace(zeroChar, '0').Replace(oneChar, '1'), 2)

let unpackRow (s : string) =
    let row = s.Substring(0, 7) |> ToBinaryWithSubstitution ('F', 'B')
    let col = s.Substring(7, 3) |> ToBinaryWithSubstitution ('L', 'R')
    (row, col)

let seatToId (row, col) = row * 8 + col

let Solve1 (fileContent : string[]) =
    fileContent
    |> Array.map(unpackRow >> seatToId)
    |> Array.max

let Solve2 (fileContent : string[]) =
    fileContent
    |> Array.map(unpackRow >> seatToId)
    |> Array.sort
    |> Seq.pairwise
    |> Seq.pick(fun (prev, next) -> if (prev + 2 = next) then Some(prev + 1) else None)