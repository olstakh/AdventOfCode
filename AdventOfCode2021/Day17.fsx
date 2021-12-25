#load "Scripts\\ParserLibrary.fsx"
open ParserLibrary

module Day17 =
    type Velocity = { vX : int; vY : int }
        with
            static member ApplyGravity v =
                { 
                    vX = if v.vX = 0 then 0 else v.vX - v.vX / (abs v.vX)
                    vY = v.vY - 1
                }

    type Probe = { X : int; Y : int; Velocity : Velocity }
        with
            static member Move probe =
                {
                    X = probe.X + probe.Velocity.vX
                    Y = probe.Y + probe.Velocity.vY
                    Velocity = probe.Velocity |> Velocity.ApplyGravity
                } 
            static member InitialWith velocity = { X = 0; Y = 0; Velocity = velocity }

    type TargetArea =
        {
            minX : int
            maxX : int
            minY : int
            maxY : int
        }
        with
            static member Within target probe =
                target.minX <= probe.X && probe.X <= target.maxX &&
                target.minY <= probe.Y && probe.Y <= target.maxY

            static member ForeverGone targetArea probe =
                probe.Velocity.vX >= 0 && probe.X > targetArea.maxX ||  // too far right and keep moving right
                probe.Velocity.vX <= 0 && probe.X < targetArea.minX ||  // too far left and keep moving left
                probe.Velocity.vY <= 0 && probe.Y < targetArea.minY     // too far below and keep moving down

    let GetHighestPointsIfHit targetArea searchArea =
        let updateHighestPoint (highestPoint, targetHit) probe =
            let highestPoint' = max highestPoint probe.Y
            let targetHit' = targetHit || TargetArea.Within targetArea probe
            (highestPoint', targetHit')

        let simulatePathUntilGone =
            Seq.unfold(fun p -> Some(p, Probe.Move p)) 
            >> Seq.takeWhile (TargetArea.ForeverGone targetArea >> not)        

        let tryGetHighestPoint =
            simulatePathUntilGone
            >> Seq.fold updateHighestPoint (0, false) >> Some
            >> Option.filter snd // filter out if target was not hit
            >> Option.map fst    // grab highest point

        [|
            for vX = searchArea.minX to searchArea.maxX do
                for vY = searchArea.minY to searchArea.maxY do
                    yield Probe.InitialWith { vX = vX; vY = vY }
        |]
        |> Array.Parallel.choose tryGetHighestPoint        

    let parseTargetArea (s:string) =
        // Example of input: target area: x=20..30, y=-10..-5
        let pTargetArea =
            pstring "target area: " >>.
            pstring "x="  >>. pint .>> pstring ".." .>>. pint .>> pstring ", " .>>
            pstring "y=" .>>. pint .>> pstring ".." .>>. pint
            |>> (fun (((minX, maxX), minY), maxY) -> { minX = minX; maxX = maxX; minY = minY; maxY = maxY })
        match run pTargetArea s with
            | Success (targetArea, _) -> targetArea
            | _ -> failwith "bad input"

    let input =
        "Input.txt"
        |> System.IO.File.ReadAllText
        
    let targetArea = parseTargetArea input
    let searchArea = { minX = -1000; maxX = +1000; minY = -1000; maxY = +1000 }

    module Task1 =
        let Answer = searchArea |> GetHighestPointsIfHit targetArea |> Array.max
    
    module Task2 =
        let Answer = searchArea |> GetHighestPointsIfHit targetArea |> Array.length
