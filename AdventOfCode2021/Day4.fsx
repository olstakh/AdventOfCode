module Day4 =

    type BingoCell =
        {
            Value : int
            Marked : bool
        }
        with
            static member Create value =
                {
                    Value = value
                    Marked = false
                }
            static member IsMarked cell = cell.Marked
            static member MarkIf value cell =
                if (cell.Value = value) then { cell with Marked = true } else cell

    type BingoBoard = BingoBoard of BingoCell[,]
        with
            static member GetSize (BingoBoard board) = board |> Array2D.length1
            static member GetBoard (BingoBoard board) = board

            static member Parse (lines : string[]) =
                let size = Array.length lines
                let parseLine (s : string) = s.Split(' ', System.StringSplitOptions.RemoveEmptyEntries) |> Array.map int
                let boardLines = lines |> Array.map parseLine
                
                Array2D.init size size (fun row col -> boardLines.[row].[col] |> BingoCell.Create)
                |> BingoBoard
            
            static member Mark number =
                BingoBoard.GetBoard
                >> Array2D.map(BingoCell.MarkIf number)
                >> BingoBoard

            static member GetRow (BingoBoard board) row =
                board[row .. row, *]
                |> Seq.cast<BingoCell>
            static member GetCol (BingoBoard board) col =
                board.[*, col .. col]
                |> Seq.cast<BingoCell>
            

            static member IsWon board =
                let allMarked = Seq.forall(BingoCell.IsMarked)
                let size = BingoBoard.GetSize board
                let rowCheck = [0 .. size - 1] |> List.exists(BingoBoard.GetRow board >> allMarked)
                let colCheck = [0 .. size - 1] |> List.exists(BingoBoard.GetCol board >> allMarked)
                rowCheck || colCheck

    let input =
        "Input.txt"
        |> System.IO.File.ReadAllLines
        |> Array.filter((<>)"")

    let numbersUnparsed = Array.head input
    let boardsUnparsed = Array.tail input

    let rec GetBoards = function
        | [||] -> []
        | lines ->  let currentBoard = lines |> Array.take 5
                    let otherBoards = lines |> Array.skip 5 
                    (BingoBoard.Parse currentBoard) :: (GetBoards otherBoards)

    let numbers = numbersUnparsed.Split(',') |> Array.map int
    let boards = GetBoards boardsUnparsed

    let FinalScore (lastWinningNumber, BingoBoard winningBoard) =
        let winningBoardScore =
            winningBoard
            |> Array2D.map(fun cell -> if cell.Marked then 0 else cell.Value)
            |> Seq.cast<int32>
            |> Seq.sum
        int64(lastWinningNumber) * int64(winningBoardScore)

    module Task1 =
        let (lastWinningNumber, lastBoards) =
            (numbers, boards)
            |> Seq.unfold(
                    fun (remainingNumbers, currentBoards) ->
                        if (List.exists (BingoBoard.IsWon) currentBoards)
                            then None // stop if at least one bingo board is finished
                            else
                                let currentNumber = Array.head remainingNumbers
                                let restOfNumbers = Array.tail remainingNumbers
                                let updatedBoards = currentBoards |> List.map (BingoBoard.Mark currentNumber)
                                Some((currentNumber, updatedBoards), (restOfNumbers, updatedBoards))
            )
            |> Seq.last

        let winningBoard =
            lastBoards
            |> List.where (BingoBoard.IsWon)
            |> List.exactlyOne

        let Answer = FinalScore (lastWinningNumber, winningBoard)

    module Task2 =
        let (lastWinningNumber, lastBoard) =
            (numbers, boards)
            |> Seq.unfold(
                fun (remainingNumbers, currentBoards) ->
                    if (List.length currentBoards = 1 && (List.exactlyOne currentBoards) |> BingoBoard.IsWon)
                        then None
                        else
                            let currentNumber = Array.head remainingNumbers
                            let restOfNumbers = Array.tail remainingNumbers
                            let updatedBoards = currentBoards |> List.where (BingoBoard.IsWon >> not) |> List.map (BingoBoard.Mark currentNumber)
                            Some ((currentNumber, updatedBoards), (restOfNumbers, updatedBoards))
            )
            |> Seq.last

        let winningBoard = lastBoard |> List.exactlyOne

        let Answer = FinalScore (lastWinningNumber, winningBoard)