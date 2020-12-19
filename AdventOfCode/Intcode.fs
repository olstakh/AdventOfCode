module Intcode

type ParameterMode =
| Position
| Immediate
| Relative

type Parameter = ParameterMode * int64

type OpcodeOperation =
| Addition of Parameter * Parameter * Parameter
| Multiplication of Parameter * Parameter * Parameter
| Input of Parameter
| Output of Parameter
| JumpIfTrue of Parameter * Parameter
| JumpIfFalse of Parameter * Parameter
| LessThan of Parameter * Parameter * Parameter
| Equals of Parameter * Parameter * Parameter
| AdjustRelativeBase of Parameter
| Halt

let toParameterMode = function | 0L -> Position | 1L -> Immediate | 2L -> Relative | bad -> failwithf "Not supported parameter mode %u" bad
let rec getDigit n = function | 1 -> n%10L | bad when bad < 1 -> failwith "Digit position must be positive" | k -> getDigit (n/10L) (k-1)

type OutputSignal = int64 list
type Program = Map<int64, int64>
type ProgramState =
    | NotActivated
    | WaitingForInput of OutputSignal * int64 * int64
    | Finished of OutputSignal
let ExtractOutputSignal = function
    | WaitingForInput(output, _, _) -> output
    | Finished(output) -> output
    | _ -> failwith "bad program state"

type ProgramResult = Program * ProgramState

let RunProgram input cursorPosition relBase (program : Program)=
    let mutable localProgram = program
    
    let get = function
        | existingKey when localProgram |> Map.containsKey existingKey -> localProgram |> Map.find existingKey
        | nonExistingKey -> 0L
    let set x = function
        | existingValue when localProgram |> Map.containsKey x -> localProgram <- localProgram |> Map.remove x |> Map.add x existingValue
        | newValue -> localProgram <- localProgram |> Map.add x newValue

    let mutable relativeBase = relBase

    let read = function
    | (Position, value) -> get value
    | (Immediate, value) -> value
    | (Relative, value) -> get (value + relativeBase)
    let write x = function
    | (Position, value) -> set value x;
    | (Immediate, _) -> failwith "Bad program, unsupported mode for write operation."
    | (Relative, value) -> set (value + relativeBase) x

    let mutable inputCopy = input |> List.toArray |> Array.copy |> Array.toList

    let parseCurrentOperation position =
        let opcode = get position
        let getParameter paramIdx =
            let paramMode = getDigit opcode (paramIdx + 2) |> toParameterMode
            let paramValue = get (position + (paramIdx |> int64))
            (paramMode, paramValue)
        match (opcode % 100L) with
        | 1L -> Addition(getParameter 1, getParameter 2, getParameter 3)
        | 2L -> Multiplication(getParameter 1, getParameter 2, getParameter 3)
        | 3L -> Input(getParameter 1)
        | 4L -> Output(getParameter 1)
        | 5L -> JumpIfTrue(getParameter 1, getParameter 2)
        | 6L -> JumpIfFalse(getParameter 1, getParameter 2)
        | 7L -> LessThan(getParameter 1, getParameter 2, getParameter 3)
        | 8L -> Equals(getParameter 1, getParameter 2, getParameter 3)
        | 9L -> AdjustRelativeBase(getParameter 1)
        | 99L -> Halt
        | bad -> failwithf "No opcode operation matches with instruction %u" bad
        

    let rec ExecuteProgram position outputs =
        let currentOperation = parseCurrentOperation position
        let currentInstruction = get position
        let nextInstruction ifNotChanged = if (get position = currentInstruction) then ifNotChanged else position

        match (currentOperation) with
        | Addition(arg1, arg2, res) ->
            write (read arg1 + read arg2) res
            ExecuteProgram (nextInstruction (position + 4L)) outputs
        | Multiplication(arg1, arg2, res) ->
            write (read arg1 * read arg2) res
            ExecuteProgram (nextInstruction (position + 4L)) outputs
        | Input(into) ->
            match (inputCopy) with
                | [] -> WaitingForInput(outputs, position, relativeBase)
                | head::tail ->
                    write head into
                    inputCopy <- tail
                    ExecuteProgram (nextInstruction (position + 2L)) outputs
        | Output(from) ->
            ExecuteProgram (nextInstruction (position + 2L)) (outputs@[read from])
        | JumpIfTrue(arg, argIfTrue) ->
            match (read arg) with
            | 0L -> ExecuteProgram (nextInstruction (position + 3L)) outputs
            | _ -> ExecuteProgram (nextInstruction (read argIfTrue)) outputs
        | JumpIfFalse(arg, argIfFalse) ->
            match (read arg) with
            | 0L -> ExecuteProgram (nextInstruction (read argIfFalse)) outputs
            | _ -> ExecuteProgram (nextInstruction (position + 3L)) outputs 
        | LessThan(arg1, arg2, res) ->
            match (read arg1 < read arg2) with
            | true -> write 1L res
            | false -> write 0L res
            ExecuteProgram (nextInstruction (position + 4L)) outputs
        | Equals(arg1, arg2, res) ->
            match (read arg1 = read arg2) with
            | true -> write 1L res
            | false -> write 0L res
            ExecuteProgram (nextInstruction (position + 4L)) outputs
        | AdjustRelativeBase(arg) ->
            relativeBase <- relativeBase + (read arg)
            ExecuteProgram (nextInstruction (position + 2L)) outputs
        | Halt -> Finished(outputs)

    let programResult = ExecuteProgram cursorPosition []
    ProgramResult(localProgram, programResult)

let ProgramFromString (str : string) = str.Split(',') |> Array.map int64 |> Array.mapi (fun ind v -> (ind |> int64, v)) |> Map.ofArray

let RunFromString input str =
    str |> ProgramFromString |> RunProgram input 0L 0L

type Amplifyer =
    {
        Program : Program;
        State: ProgramState;
        InputSignals: int64 list;
    } with
    member this.getCursorposition =
        match (this.State) with
        | NotActivated -> 0L
        | WaitingForInput(_, position, _) -> position
        | Finished _ -> failwith "Program had already halted"
    member this.getRelativebase =
        match (this.State) with
        | NotActivated -> 0L
        | WaitingForInput(_, _, relBase) -> relBase
        | Finished _ -> failwith "Program had already halted"
    member this.Execute input =
        let (newProgram, newState) = RunProgram (this.InputSignals @ input) (this.getCursorposition) (this.getRelativebase) this.Program
        {
            Program = newProgram
            State = newState
            InputSignals = []
        }

let distrib e L =
    let rec aux pre post = 
        seq {
            match post with
            | [] -> yield (L @ [e])
            | h::t -> yield (List.rev pre @ [e] @ post)
                      yield! aux (h::pre) t 
        }
    aux [] L

let rec perms = function 
    | [] -> Seq.singleton []
    | h::t -> Seq.collect (distrib h) (perms t)

let CalcAmplifyers programStr input phases =
    phases
    |> List.fold(fun acc phase -> RunFromString [phase; acc] programStr |> snd |> ExtractOutputSignal |> List.head) input

let CalcAplifyersFeedback programStr input phases =
    let createAmplifyer ph =
        {
            Program = programStr |> ProgramFromString;
            State = NotActivated;
            InputSignals = [ph];
        }

    let amplifyers = phases |> List.toArray |> Array.map createAmplifyer

    let runAmplifyer (ampIdx, inputSignal) =
        printfn "Running amplifyer %d" ampIdx
        let amp = Array.get amplifyers ampIdx

        let updatedAmp = amp.Execute inputSignal

        do Array.set amplifyers ampIdx updatedAmp

        match ((Array.get amplifyers ampIdx).State) with
            | WaitingForInput(output, _, _) -> Some((ampIdx, inputSignal), ((ampIdx + 1) % (Array.length amplifyers), output))
            | Finished _ when ampIdx + 1 = Array.length amplifyers -> None
            | Finished output -> Some((ampIdx, inputSignal), ((ampIdx + 1) % (Array.length amplifyers), output))
            | _ -> failwith "impossible"


    let a = Seq.unfold runAmplifyer (0, [input]) |> Seq.length
    printfn "Amount of runs: %d" a
    (amplifyers |> Array.last).State |> ExtractOutputSignal |> List.rev |> List.head
   
let Input =
    System.IO.File.ReadAllLines("C:\Users\olstakh\Source\Repos\AdventOfCode\AdventOfCode\Input.txt") |> Array.head



(*
type RobotDirection = | Up | Down | Left | Right
let robotMove (x, y) = function
    | Up -> (x, y+1) | Down -> (x, y-1)
    | Left -> (x-1, y) | Right -> (x+1, y)
let robotTurnLeft = function | Up -> Left | Left -> Down | Down -> Right | Right -> Up
let robotTurnRight = function | Up -> Right | Right -> Down | Down -> Left | Left -> Up


let moveUntilHalted ((robotPos, robotDirection, mapOfPainted, amplifyer) as currentState) =
    let colorOfCurrentBlock =
        match (Map.tryFind robotPos mapOfPainted) with
        | None -> 0
        | Some color -> color
        |> int64

    let (updatedProgram, programOutput) = RunProgram [colorOfCurrentBlock] (amplifyer.State |> getCursorposition) (amplifyer.State |> getRelativebase) (amplifyer.Program)
    let direction = ExtractOutputSignal programOutput

    let updatedMap =
        match (direction) with
        | [color;_] when Map.containsKey robotPos mapOfPainted -> mapOfPainted |> Map.remove robotPos |> Map.add robotPos (color|>int)
        | [color;_] -> mapOfPainted |> Map.add robotPos (color |> int)
        | _ -> failwithf "bad program output %A" direction

    let updatedRobotDirection =
        match (direction) with
        | t when List.length t <> 2 -> failwithf "bad direction %A" t
        | [_;0L] -> robotDirection |> robotTurnLeft
        | [_;1L] -> robotDirection |> robotTurnRight
        | _ -> failwithf "bad program output %A" direction

    let updatedRobotPos = updatedRobotDirection |> robotMove robotPos
    let updatedAmp =
        {
            Program = updatedProgram;
            State = programOutput;
            InputSignals = []
        }

    let updatedIterationState = (updatedRobotPos, updatedRobotDirection, updatedMap, updatedAmp)

    match (programOutput) with
    | WaitingForInput _ -> Some (currentState, updatedIterationState)
    | Finished _ -> None
    | _ -> failwith "bad program output"

let (finalPos, finalDir, finalMap, _) = Seq.unfold moveUntilHalted ((0,0),Up,Map.empty |> Map.add (0,0) 1, { Program = Input |> ProgramFromString; State = NotActivated; InputSignals=[]}) |> Seq.last

let (minX, minY) = finalMap |> Map.fold(fun (_minX, _minY) (currX, currY) _ -> ((min _minX currX), (min _minY currY))) (0, 0)
let shiftedMap = finalMap |> Map.toArray |> Array.map(fun ((cx, cy), color) -> ((cx-minX, cy-minY), color)) |> Map.ofArray

let (maxX, maxY) = shiftedMap |> Map.fold(fun (_minX, _minY) (currX, currY) _ -> ((max _minX currX), (max _minY currY))) (0, 0)

let plane =
    [0 .. maxY]
    |> List.map(fun y ->
        [0 .. maxX]
        |> List.map(fun x -> match (Map.tryFind (x,y) shiftedMap) with | None | Some 0 -> '.' | Some 1 -> '#')
        |> List.toArray
        |> System.String)

finalMap |> Map.count

*)


type TileType = | Empty | Wall | Block | HorizontalPaddle | Ball
type ArcadeOutput =
    | Score of int64
    | Drawing of int64 * int64 * TileType

let getScore = function | Score score -> Some score | _ -> None
let getDrawing = function | Drawing (x,y,t) -> Some (x,y,t) | _ -> None
let isOfType (c:TileType) = function | (_, _, t) when t = c -> true | _ -> false

let toTile = function
    | 0L -> Empty | 1L -> Wall | 2L -> Block | 3L -> HorizontalPaddle | 4L -> Ball | _ -> failwith "Unsupported tile type"

let toArcadeOutput = function
    | (-1L, 0L, score) -> Score(score)
    | (x, y, tile) -> Drawing(x, y, tile |> toTile)

let parseArcadeOutput = function
    | badList when (badList |> List.length) % 3 > 0 -> failwith "bad program output"
    | lst -> let cnt = (lst |> List.length) / 3
             lst |> List.splitInto cnt |> List.map(fun [x;y;t] -> (x,y,t) |> toArcadeOutput)

type JoystickPosition = Neutral | TiltedLeft | TiltedRight
let toArcadeInput = function
    | Neutral -> 0L
    | TiltedLeft -> -1L
    | TiltedRight -> +1L

let rec GetBestScore (paddlePos, ballPos) (amp:Amplifyer) =
    let joystickInput =
        match (paddlePos, ballPos) with
        | (a, b) when a > b -> TiltedLeft
        | (a, b) when a < b -> TiltedRight
        | _ -> Neutral
        |> toArcadeInput

    let newAmp = amp.Execute [joystickInput]

    let getLastDrawing lst t =
        lst |> parseArcadeOutput |> List.choose getDrawing |> List.filter(isOfType t) |> List.tryLast

    match (newAmp.State) with
    | NotActivated -> failwith "impossible"
    | Finished output -> output |> parseArcadeOutput |> List.choose getScore |> List.last
    | WaitingForInput (currentOutput,_,_) -> let newPaddlePosition = getLastDrawing currentOutput HorizontalPaddle |> Option.map(fun (x,_,_) -> x) |> Option.defaultValue paddlePos
                                             let newBallPosition = getLastDrawing currentOutput Ball |> Option.map(fun (x,_,_) -> x) |> Option.defaultValue ballPos
                                             GetBestScore (newPaddlePosition, newBallPosition) newAmp
                                            
GetBestScore (0L,0L) { Program = Input |> ProgramFromString; State = NotActivated; InputSignals = []}
