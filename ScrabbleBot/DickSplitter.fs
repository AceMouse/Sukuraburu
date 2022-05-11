module ScrabbleBot.DickSplitter

open System.Collections.Generic
open System.IO

// Splits a dictionary into multiple dictionaries by length and point values

let splitDictionary dic =
    let splitDic = Dictionary<int, Dictionary.Dict>()
    File.ReadLines dic |> Seq.iter (fun x ->
        for i in 0 .. x.Length do
            let v = splitDic.TryGetValue i
            if fst v
            then splitDic.Add (i, (Dictionary.insert x (snd v)))
            else splitDic.Add (i, Dictionary.insert x (Dictionary.empty ()))
        )
    splitDic
