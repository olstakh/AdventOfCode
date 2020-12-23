module Day14

open Common
open ParserLibrary

type MemoryAddress = MemoryAddress of int64
type MemoryValue = MemoryValue of int64 with static member Extract (MemoryValue v) = v

type Action =
    | UpdateMask of string
    | UpdateMemory of MemoryAddress * MemoryValue
    with
        static member Parse (s : string) =
            let updateMaskParser =
                let maskString = manyChars (anyOf ['0'; '1'; 'X'])
                pstring "mask" .>> pstring " = " >>. maskString |>> UpdateMask
            let updateMemoryParser =
                pstring "mem" >>. between (pchar '[') (pint64 |>> MemoryAddress) (pchar ']') .>> pstring " = " .>>. (pint64 |>> MemoryValue) |>> UpdateMemory
            
            match (run (choice [updateMaskParser; updateMemoryParser]) s) with
                | Success (parsedAction, _) -> parsedAction
                | Failure (_, error, position) -> failwithf "Can't parse the string %s. Error %s. Position: %d" s error position.column

type Memory =
    {
        Mask : string
        Memory : Map<MemoryAddress, MemoryValue>
    } with
        static member Empty = { Mask = ""; Memory = Map.empty }
        static member GetMemory mem = mem.Memory

let Solve applyBitFunc applyTo updateResultWith =

    let ApplyMask value =
        let applyToAll s p = Seq.collect(applyBitFunc p) s
        Seq.rev >> Seq.indexed >> Seq.fold applyToAll (Seq.singleton value)

    let ExecuteAction mem = function
        | UpdateMask newMask -> { mem with Mask = newMask }
        | UpdateMemory (key, value) -> { mem with Memory = mem.Memory
                                                           |> AddOrUpdateValues (ApplyMask (applyTo (key, value)) mem.Mask
                                                           |> Seq.map(updateResultWith (key, value)))
                                       }
    
    Array.map (Action.Parse)
    >> Array.fold ExecuteAction Memory.Empty
    >> Memory.GetMemory
    >> Map.toList
    >> List.sumBy(snd >> MemoryValue.Extract)

let Solve1 (fileContent : string[]) =
    let applyBit (ind, ch) n =
        let bitMask = 1L <<< ind
        seq {
            match (ch) with
            | '0' -> yield (n &&& (~~~bitMask))
            | '1' -> yield (n ||| (   bitMask))
            | 'X' -> yield n
            | c -> failwithf "Unexpected mask characted %c" c
        }

    let ApplyTo (MemoryAddress key, MemoryValue value) = value
    let UpdateResult (MemoryAddress key, _) result = (MemoryAddress key, MemoryValue result)

    Solve applyBit ApplyTo UpdateResult fileContent

let Solve2 (fileContent : string[]) =
    let applyBit (ind, ch) n =
        let bitMask = 1L <<< ind
        seq {
                match (ch) with
                | '0' -> yield n
                | '1' -> yield n ||| bitMask
                | 'X' -> yield (n ||| bitMask); yield (n &&& (~~~bitMask))
                | c -> failwithf "Unexpected mask characted %c" c
            }

    let ApplyTo (MemoryAddress key, MemoryValue value) = key
    let UpdateResult (_, MemoryValue value) result = (MemoryAddress result, MemoryValue value)

    Solve applyBit ApplyTo UpdateResult fileContent