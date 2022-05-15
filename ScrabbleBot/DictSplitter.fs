module ScrabbleBot.DickSplitter

open System.IO

// Splits a dictionary into multiple dictionaries by length and point values

let splitDictionary dic : Dictionary.Dict list =
    let rec aux (dic : string list) (tl : Map<int, Dictionary.Dict>) =
        match dic with
        | x::xs -> 
            let v = tl.TryGetValue x.Length
            let rec insert w (d : Map<int, Dictionary.Dict>) acc =
                match acc with
                | 0 -> d
                | _ ->
                    if fst v
                    then insert w (d.Add (x.Length, Dictionary.insert w (snd v))) (acc - 1)
                    else insert w (d.Add (x.Length, Dictionary.insert w (Dictionary.empty ()))) (acc - 1)
            let res = insert x tl x.Length
            aux xs res
        | [] -> tl.Values |> List.ofSeq
    aux (File.ReadLines dic |> List.ofSeq) Map.empty
