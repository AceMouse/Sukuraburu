module ScrabbleBot.DickSplitter

open System.IO

// Splits a dictionary into multiple dictionaries by length and point values

(*let splitDictionary dic : Map<int,Dictionary.Dict> =
    let rec aux (dic : string list) (tl : Map<int, Dictionary.Dict>) =
        match dic with
        | x::xs -> 
            let v = tl.TryGetValue x.Length
            let rec insert w (d : Map<int, Dictionary.Dict>) acc =
                match acc with
                | 0 -> d
                | _ ->
                    if fst v
                    then insert w (d.Add (acc, Dictionary.insert w (snd v))) (acc - 1)
                    else insert w (d.Add (acc, Dictionary.insert w (Dictionary.empty ()))) (acc - 1)
            let res = insert x tl x.Length
            aux xs res
        | [] -> tl
    aux (File.ReadLines dic |> List.ofSeq) Map.empty*)
    
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
