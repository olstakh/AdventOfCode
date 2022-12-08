#load "Scripts\\Utils.fsx"
open Utils

module Day8 =

    let data = Input()
    
    let height = Array.length data
    let width = String.length (Array.head data)

    let rec VisibleTrees outOfBounds tree (dr, dc) = function
        | r, c when r < 0 || c < 0 || r >= height || c >= width -> outOfBounds
        | r, c when data[r][c] >= tree -> 1
        | r, c -> 1 + VisibleTrees outOfBounds tree (dr, dc) (r + dr, c + dc)    

    module Task1 =

        let Answer = 
            [
                for i = 0 to height - 1 do
                    for j = 0 to width - 1 do
                        if  VisibleTrees 1 (data[i][j]) (-1, 0) (i - 1, j) = i + 1 ||
                            VisibleTrees 1 (data[i][j]) (+1, 0) (i + 1, j) = height - i ||
                            VisibleTrees 1 (data[i][j]) (0, -1) (i, j - 1) = j + 1 ||
                            VisibleTrees 1 (data[i][j]) (0, +1) (i, j + 1) = width - j
                        then yield (i, j)
            ]
            |> List.length

    module Task2 =

        let Answer =
            [
                for i = 0 to height - 1 do
                    for j = 0 to width - 1 do
                        yield (VisibleTrees 0 (data[i][j]) (-1, 0) (i - 1, j)) *
                              (VisibleTrees 0 (data[i][j]) (+1, 0) (i + 1, j)) *
                              (VisibleTrees 0 (data[i][j]) (0, -1) (i, j - 1)) *
                              (VisibleTrees 0 (data[i][j]) (0, +1) (i, j + 1))
            ]
            |> List.max