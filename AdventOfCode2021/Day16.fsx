#load "Scripts\\ParserLibrary.fsx"
open ParserLibrary

module Day16 =

    let BinaryStringToInt (s:string) = System.Convert.ToInt32(s, 2)
    let BinaryStringToInt64 (s:string) = System.Convert.ToInt64(s, 2)
    let HexStringToInt (s:string) = System.Convert.ToInt32(s, 16)

    let HexCharToBinaryString (ch : char) =
        let number = HexStringToInt (string ch)
        System.Convert.ToString(number, 2).PadLeft(4, '0')

    let HexStringToBinaryString (s:string) =
        s.ToCharArray()
        |> Array.map HexCharToBinaryString
        |> System.String.Concat    

    let pBinaryString ofLen = parseByCount binaryChar ofLen |>> charListToStr <?> sprintf "Binary string of length %d" ofLen
    let pBinaryInt ofLen = pBinaryString ofLen |>> BinaryStringToInt <?> sprintf "Binary string of length %d, converted to an integer" ofLen

    type Operation =
        | Sum | Product | Minimum | Maximum | GreaterThan | LessThan | EqualTo
        with
            static member fromInt = function
                | 0 -> Sum | 1 -> Product | 2 -> Minimum | 3 -> Maximum | 5 -> GreaterThan | 6 -> LessThan | 7 -> EqualTo
                | x -> failwithf "Unsupported operation id %d" x
                
            static member Apply operation values =
                // Applies given funtion to the first 2 values from the list
                let applyFor2Values f =
                    match values with
                    | [firstValue; secondValue] -> f firstValue secondValue
                    | _ -> failwithf "Operation not supported, was expecting exactly 2 values, given %d values" (List.length values)

                match (operation) with
                | Sum -> values |> List.reduce (+)
                | Product -> values |> List.reduce (*)
                | Minimum -> values |> List.min
                | Maximum -> values |> List.max
                | GreaterThan -> applyFor2Values (fun a b -> if a > b then 1L else 0L)
                | LessThan -> applyFor2Values (fun a b -> if a < b then 1L else 0L)
                | EqualTo -> applyFor2Values (fun a b -> if a = b then 1L else 0L)
                
    type Packet =
        {
            Version : int
            Content : PacketContent
        }
    and PacketContent =
        | Literal of int64
        | Subpackets of (Operation * Packet List)

    let pBinaryBlocks =
        let label = "Parser of 5 bit blocks abbbb, where bbbb is a value and if a is 0 - stop reading, otherwise read next block and so on, combining the values"
        let (pBlocks, pBlocksRef) = createParserForwardedToRef<string>()
        pBlocksRef.Value <- choice [
            pchar '0' >>. pBinaryString 4 // last packet
            pchar '1' >>. pBinaryString 4 (* keep reading and concat all *) .>>. pBlocks |>> System.String.Concat
        ]
        pBlocks
        |>> BinaryStringToInt64
        <?> label

    let pPacket =
        let label = "Packet is 3 bits version, 3 bits id type, other content, depending on id type"

        // This parser is recursive, so need
        let (pPacketParser, pPacketParserRef) = createParserForwardedToRef<Packet>()
        
        let pVersion = pBinaryInt 3
        
        let rec pSubPackets =
            binaryChar >>=
                function
                    | '0'   -> pBinaryInt 15 >>= parseBySize pPacketParser
                    |  _    -> pBinaryInt 11 >>= parseByCount pPacketParser

        let pContent =
            let pTypeId = pBinaryInt 3
            pTypeId >>=
                function
                    | 4 -> pBinaryBlocks |>> Literal
                    | op -> pSubPackets |>> fun packets -> Subpackets(Operation.fromInt op, packets)

        pPacketParserRef.Value <-
            pVersion .>>. pContent
            |>> fun (version, content) -> { Version = version; Content = content }
        pPacketParser
        <?> label

    let input =
        "Input.txt"
        |> System.IO.File.ReadAllText

    let Solve packetValueFn =
        match (run pPacket (HexStringToBinaryString input)) with
            | Success (packet, _) -> packetValueFn packet
            | Failure (label, error, pos) -> failwithf "Failed to parse input. Label: %s, Error : %s, position : %d" label error pos.column

    module Task1 =
        let rec AddVersionsOfPackets (p : Packet) =
            p.Version +
            match (p.Content) with
                | Literal _ -> 0
                | Subpackets (operation, subPackets) -> subPackets |> List.sumBy AddVersionsOfPackets

        let Answer = Solve AddVersionsOfPackets

    module Task2 =
        let rec PacketValue (p : Packet) =
            match (p.Content) with
                | Literal v -> v
                | Subpackets (operation, packets) ->
                    packets
                    |> List.map PacketValue
                    |> Operation.Apply operation

        let Answer = Solve PacketValue