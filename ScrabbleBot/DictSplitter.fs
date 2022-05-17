module ScrabbleBot.DickSplitter

open System.IO

// Splits a dictionary into multiple dictionaries by length and point values

let splitDictionary dic : Map<int,Dictionary.Dict> =
    let rec aux words (map : Map<int,Dictionary.Dict>) =
        match words with
        | [] -> map
        | x::xs ->
            let rec aux2 i map =
                if i = 0 then Map.add i (Dictionary.insert x (Map.find i map)) map
                else aux2 (i-1) (Map.add i (Dictionary.insert x (Map.find i map)) map)
            aux xs (aux2 (String.length x) map)
    
    aux (File.ReadLines dic |> List.ofSeq) (Map.ofList (List.init 20 (fun i -> (i, Dictionary.empty()))))
