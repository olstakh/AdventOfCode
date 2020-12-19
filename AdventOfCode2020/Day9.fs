module Day9

let canSum2Numbers numbers expected =
    // Sorry for O^2
    numbers |> Seq.exists(fun a -> numbers |> Seq.contains(expected - a))

let splitAndCheck numbers =
    let expected = numbers |> Seq.last
    canSum2Numbers (numbers |> Seq.take(Seq.length numbers - 1)) expected

let findFirstThatFails window = Seq.windowed (window + 1) >> Seq.find(splitAndCheck >> not)

let Solve1 (fileContent : string[]) =
    fileContent
    |> Array.map int64
    |> findFirstThatFails 25
    |> Seq.last

let tryAddUpFromStart expectedSum =
    Seq.scan (+) 0L
    >> Seq.skip 1
    >> Seq.tryFindIndex ((=)expectedSum)

let tryAddUp expectedSum start =
    Seq.skip start
    >> tryAddUpFromStart expectedSum
    >> Option.map(fun finish -> (start, finish + start))

let Solve2 (fileContent : string[]) =
    let solve1Ans = fileContent |> Solve1

    let numbers = fileContent |> Array.map int64

    let (start, finish) = [0 .. numbers.Length - 1] |> List.pick(fun start -> tryAddUp solve1Ans start numbers)
    let segment = numbers |> Array.skip start |> Array.take (finish - start + 1)

    Array.min segment + Array.max segment