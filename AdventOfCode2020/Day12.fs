module Day12

open Common

let normalizeAngle a = (a % 360 + 360) % 360
let repeatForAngle a func =
    let repeatTurns = normalizeAngle (a / 90)
    Seq.init repeatTurns (fun turn -> func) |> Seq.reduce (>>)

type Orientation =
    | East | West | North | South
    with
        static member RotateClockwise = function
            | East -> South | South -> West | West -> North | North -> East

let RotateCoordClockwise (c : Coord) = { X = c.Y; Y = -c.X }

let MoveCoord (c : Coord) dist = function
    | East -> { c with X = c.X + dist }
    | West -> { c with X = c.X - dist }
    | North -> { c with Y = c.Y + dist }
    | South -> { c with Y = c.Y - dist }

type Action =
    | Move of Orientation
    | MoveForward
    | TurnLeft
    | TurnRight
    with static member Parse = function
            | 'N' -> Move North
            | 'S' -> Move South
            | 'E' -> Move East
            | 'W' -> Move West
            | 'F' -> MoveForward
            | 'L' -> TurnLeft
            | 'R' -> TurnRight
            | c -> failwithf "Unsupported action %c" c

type Instruction = Instruction of Action * int
    with static member Parse (s : string) = Instruction(s.[0] |> Action.Parse, s.Substring(1) |> int)

type Ship =
    {
        Base : Coord
        Waypoint : Coord
        Orientation : Orientation
    } with
        static member Manhattan ship = ship.Base |> Coord.Manhattan
        static member Initial = { Base = Coord.Origin; Orientation = East; Waypoint = { X = 10; Y = 1} }

let SolveWith instructionExecuter = 
    Array.map (Instruction.Parse)
    >> Array.fold instructionExecuter Ship.Initial
    >> Ship.Manhattan

let Solve1 (fileContent : string[]) =

    let rec ApplyInstruction ship = function
        | Instruction(Move direction, dist) -> { ship with Base = MoveCoord ship.Base dist direction }
        | Instruction(MoveForward, dist) -> ApplyInstruction ship (Instruction (Move ship.Orientation, dist))
        | Instruction(TurnRight, angle) -> { ship with Orientation = repeatForAngle angle Orientation.RotateClockwise ship.Orientation }
        | Instruction(TurnLeft, angle) -> ApplyInstruction ship (Instruction(TurnRight, 360 - angle))

    fileContent
    |> SolveWith ApplyInstruction

let Solve2 (fileContent : string[]) =

    let rec ApplyInstruction ship = function
        | Instruction(Move direction, dist) -> { ship with Waypoint = MoveCoord ship.Waypoint dist direction }
        | Instruction(MoveForward, cnt) -> { ship with Base = ship.Base + (ship.Waypoint * cnt) }
        | Instruction(TurnRight, angle) -> { ship with Waypoint = repeatForAngle angle RotateCoordClockwise ship.Waypoint }
        | Instruction(TurnLeft, angle) -> ApplyInstruction ship (Instruction(TurnRight, 360 - angle))
    
    fileContent
    |> SolveWith ApplyInstruction
