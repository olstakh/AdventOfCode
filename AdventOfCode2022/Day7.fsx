#load "Scripts\\ParserLibrary.fsx"
#load "Scripts\\Utils.fsx"
open Utils
open ParserLibrary

module Day7 =

    let nameParser = manyChars (anyOf (['a'..'z'] @ ['.';'/']))

    type File =
        {
            Size : int
            Name : string
        }
        with
            static member Parser =
                pint .>> whitespaceChar .>>. nameParser
                |>> fun (size, fileName) -> { Name = fileName; Size = size }
            static member FileSize fi = fi.Size

    type Command = 
        | ChangeDirectory of string
        | List
        with
            static member Parser =
                let changeDirParser = pstring "cd" .>> whitespaceChar >>. nameParser |>> ChangeDirectory
                let listParser = pstring "ls" |>> fun _ -> List

                pchar '$' >>. whitespaceChar >>. choice [
                    changeDirParser
                    listParser
                ]

    type TerminalOutput =
        | Command of Command
        | FileInfo of File
        | DirInfo of string
        with
            static member Parser =
                let commandParser = Command.Parser |>> Command
                let fileInfoParser = File.Parser |>> FileInfo
                let dirInfoParser = pstring "dir" >>. whitespaceChar >>. nameParser |>> DirInfo

                choice [
                    commandParser
                    fileInfoParser
                    dirInfoParser
                ]

    let mutable fileSystem : Map<string, File list> = Map.empty
    let addFile path file =
        fileSystem <- Map.change path (Option.defaultValue [] >> List.append [file] >> Some) fileSystem

    let tryGoUp (path:string) =
        let lastBreak = path.TrimEnd('/').LastIndexOf('/')
        if (lastBreak > -1) then
            Some (path.Substring(0, lastBreak + 1))
        else
            None

    let rec ProcessTerminalOutput (currentFolder : string) = function
        | Command (ChangeDirectory "/") -> "/"
        | Command (ChangeDirectory "..") -> (tryGoUp currentFolder).Value // input is guaranteed to be correct
        | Command (ChangeDirectory subDir) -> currentFolder + subDir + "/"
        | Command List -> currentFolder
        | FileInfo file ->
            do addFile currentFolder file
            currentFolder
        | DirInfo dir -> currentFolder

    let finalFolder =
        Input()
        |> Array.map (ParseLine (TerminalOutput.Parser))
        |> Array.fold ProcessTerminalOutput "/"

    let mutable directorySize : Map<string, int> = Map.empty
    let addSize path size =
        directorySize <- Map.change path (Option.defaultValue 0 >> (+)size >> Some) directorySize

    let rec PropagateSize path files =
        do addSize path (files |> List.sumBy (File.FileSize))
        tryGoUp path |> Option.iter (fun parent -> PropagateSize parent files)

    do Map.iter PropagateSize fileSystem
    let directoriesSizes = Map.values directorySize |> Seq.sort

    module Task1 =
        let sizeLimit = 100000
        let Answer = directoriesSizes |> Seq.filter(fun sz -> sz < sizeLimit) |> Seq.sum

    module Task2 =
        let usedSpace = directoriesSizes |> Seq.max
        let unusedSpace = 70000000 - usedSpace
        let unusedSpaceGoal = 30000000
        let Answer = directoriesSizes |> Seq.find(fun sz -> unusedSpace + sz >= unusedSpaceGoal)
    