module Day8

open System

type Operation = Acc | Jmp | Nop
    with
    static member Parse = function
        | "acc" -> Acc | "jmp" -> Jmp | "nop" -> Nop
        | unknown -> failwithf "unknown operatoin %s" unknown

type Argument = Argument of int
    with
    static member Parse = Int32.Parse >> Argument

type Instruction = Instruction of Operation * Argument
    with
    static member Parse (s : string) =
        let [| operationStr; argumentStr |] = s.Split(' ')
        Instruction(Operation.Parse operationStr, Argument.Parse(argumentStr))

type Registry =
    {
        CurrentInstruction : int
        Accumulator : int        
    } with
    static member CalculateOffset = function
        | Instruction(Acc, Argument acc) -> (+1, acc)
        | Instruction(Jmp, Argument jmp) -> (jmp, 0)
        | Instruction(Nop, Argument _) -> (+1, 0)
    static member ApplyOffset r (offsetPosition, offsetAcc) =
        {
            CurrentInstruction = r.CurrentInstruction + offsetPosition
            Accumulator = r.Accumulator + offsetAcc
        }
    static member Calculate registry = Registry.CalculateOffset >> (Registry.ApplyOffset registry)
    static member Default =
        {
            CurrentInstruction = 0
            Accumulator = 0
        }
    static member GetAccumulator s = s.Accumulator
    static member GetPosition s = s.CurrentInstruction

type Machine =
    {
        Program : Instruction list
        Registry : Registry
        RegistryHistory : Registry list
    } with
        static member ExecuteCurrentInstruction p =
            let registry = p.Registry

            let newRegistry =
                if Machine.ReachedEnd p
                then registry
                else p.Program |> List.item (registry.CurrentInstruction) |> Registry.Calculate registry

            { p with
                Registry = newRegistry
                RegistryHistory = registry :: p.RegistryHistory
            }

        static member ReachedEnd p =
            p.Registry.CurrentInstruction = p.Program.Length

        static member ReachedCycle p =
            p.RegistryHistory |> List.map (Registry.GetPosition) |> List.contains(p.Registry.CurrentInstruction)

        static member CreateFromInstructions instructions =
            {
                Program = instructions
                Registry = Registry.Default
                RegistryHistory = []
            }
        static member CreateFrom = List.map(Instruction.Parse) >> Machine.CreateFromInstructions

        static member GetAccumulator s = s.Registry.Accumulator

let run initial =
    Seq.unfold(fun s -> Some(s, Machine.ExecuteCurrentInstruction s)) initial
    |> Seq.takeWhile(Machine.ReachedCycle >> not)
    |> Seq.last

let Solve1 (fileContent : string[]) =
    run (Machine.CreateFrom (Array.toList fileContent))
    |> Machine.GetAccumulator

let Solve2 (fileContent : string[]) =
    let instructions = fileContent |> Array.map(Instruction.Parse) |> Array.toList

    let allChanges =
                    seq {
                        for i in 0 .. (instructions.Length - 1) do
                            match (instructions |> List.item i) with
                            | Instruction(Jmp, Argument arg) -> yield (Common.listWithOneItemChanged instructions i (Instruction(Nop, Argument arg)))
                            | Instruction(Nop, Argument arg) -> yield (Common.listWithOneItemChanged instructions i (Instruction(Jmp, Argument arg)))
                            | _ -> ()
                    }

    allChanges
    |> Seq.map(Machine.CreateFromInstructions >> run)
    |> Seq.filter(Machine.ReachedEnd)
    |> Seq.exactlyOne
    |> Machine.GetAccumulator
