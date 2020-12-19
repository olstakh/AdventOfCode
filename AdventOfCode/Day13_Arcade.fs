module Day13_Arcade

type TileType = | Empty | Wall | Block | HorizontalPaddle | Ball
type ArcadeOutput =
    | Score of int64
    | Drawing of int64 * int64 * TileType

let getScore = function | Score score -> Some score | _ -> None

let toTile = function
    | 0L -> Empty | 1L -> Wall | 2L -> Block | 3L -> HorizontalPaddle | 4L -> Ball | _ -> failwith "Unsupported tile type"

let toArcadeOutput = function
    | (-1L, 0L, score) -> Score(score)
    | (x, y, tile) -> Drawing(x, y, tile |> toTile)

let isBlock = function | Block -> true | _ -> false
let parseArcadeOutput = function
    | badList when (badList |> List.length) % 3 > 0 -> failwith "bad program output"
    | lst -> let cnt = (lst |> List.length) / 3
             lst |> List.splitInto cnt |> List.map(fun [x;y;t] -> (x,y,t) |> toArcadeOutput)

type JoystickPosition = Neutral | TiltedLeft | TiltedRight
let toArcadeInput = function
    | Neutral -> [0L]
    | TiltedLeft -> [-1L]
    | TiltedRight -> [+1L]

let rec GetBestScore amp =
    match (amp.State) with
    | Finished output -> output |> parseArcadeOutput |> List.choose getScore |> List.last
    | _ -> let scoreWhenNeutral = Get
