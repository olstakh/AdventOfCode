#load "Scripts\\ParserLibrary.fsx"
#load "Scripts\\Utils.fsx"
open Utils
open ParserLibrary

module Day4 =

    type Segment =
        {
            Start : int
            Finish : int
        } with
            static member Construct (start, finish) =
                {
                    Start = start
                    Finish = finish
                }
            // Ex: 10-20
            static member Parser =
                pint .>> pchar '-' .>>. pint |>> Segment.Construct
                <?> "Segment"

    type SegmentPair =
        {
            Segment1 : Segment
            Segment2 : Segment
        } with
            static member Construct (segment1, segment2) =
                {
                    Segment1 = segment1
                    Segment2 = segment2
                }
            // Ex: 10-20,25-35
            static member Parser =
                Segment.Parser .>> pchar ',' .>>. Segment.Parser |>> SegmentPair.Construct
                <?> "Segment pair"

    let InputData =
        Input()
        |> Array.map (ParseLine (SegmentPair.Parser))
 
    let Solve filterCondition =
        InputData
        |> Array.filter filterCondition
        |> Array.length

    module Task1 =
        let oneSegmentFullyContainsAnother segmentPair =
            segmentPair.Segment1.Start <= segmentPair.Segment2.Start && segmentPair.Segment2.Finish <= segmentPair.Segment1.Finish
            ||
            segmentPair.Segment2.Start <= segmentPair.Segment1.Start && segmentPair.Segment1.Finish <= segmentPair.Segment2.Finish

        let Answer = Solve oneSegmentFullyContainsAnother

    module Task2 =
        let nonOverlapCheck segmentPair =
            segmentPair.Segment1.Start > segmentPair.Segment2.Finish || segmentPair.Segment1.Finish < segmentPair.Segment2.Start
            ||
            segmentPair.Segment2.Start > segmentPair.Segment1.Finish || segmentPair.Segment2.Finish < segmentPair.Segment1.Start
        
        let Answer = Solve (nonOverlapCheck >> not)