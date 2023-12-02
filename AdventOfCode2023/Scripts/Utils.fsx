module Utils

    let Input() =
        System.IO.Path.Combine(__SOURCE_DIRECTORY__, "..\\Input.txt")
        |> System.IO.File.ReadAllLines