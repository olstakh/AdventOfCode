module Common

open System

let addToLastOrAppend (head::tail as lst) line =
    if (String.IsNullOrEmpty(line))
    then ""::lst // append
    else (head + " " + line)::tail // add to last

let listWithOneItemChanged lst idx v =
    lst |> List.mapi(fun ind u -> if ind = idx then v else u)