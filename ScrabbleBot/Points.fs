
module internal ScrabbleBot.Points

    open Eval
    open StateMonad

    // a 1 -> DL
    // b 2 -> TL
    // c 3 -> DW
    // d 4 -> TW
    let printHand (hand : (uint32 * Set<char*int>) list) =
        hand |>
        List.fold (fun _ (i, s) -> System.Console.WriteLine (sprintf "%d -> (%A)" i s )) ()
    let rec tilePoints (usedMask:byte) (hand : (uint32 * Set<char*int>) list) lst i =
                    //printHand hand
        if (((usedMask &&& (1uy<<<i)) = 0uy) && (i < hand.Length))
        then
            let setList = hand[i] |> snd |> Set.toList
            let l = List.init setList.Length (fun j -> (i,((fst hand[i]),setList[j]))) @ lst
            tilePoints usedMask hand l (i+1)
        else if (i < hand.Length)
        then
            tilePoints usedMask hand lst (i+1)
        else
            lst
            
    let calculatePoints (squares : square list) (word : word) =
            // Partially apply square functions to word
            let partiallyApplied = List.mapi (fun i x -> List.map (fun (p,sf) -> (p, sf word i)) (Map.toList x)) squares
            // Flatten lists into a single list
            let flattened = List.fold (fun s t -> s @ t) [] partiallyApplied
            // Sort by priority
            let sorted = List.sortBy fst flattened
            // Discard priority
            let squareFunctions = List.map snd sorted
            // Map function to get result
            let squareFunctions = List.map (fun x -> fun t ->
                                            match x t with
                                            | Success v -> v
                                            | Failure _ -> 0
                                    ) squareFunctions
            
            // Compose square functions
            let composedFunction = List.fold (fun s f -> s >> f) id squareFunctions
            composedFunction 0
            
    let getMovePoints (squares : int*int -> square) (move : ((int*int)*(uint32*(char*int))) list) (tiles : ((int * int) * (uint32 * (char * int))) list) (words : word list) : int = 
        let wordLengthPoints = if move.Length = 7 then 50 else 0
        let squares = List.map (fun (coord, _) -> squares coord) tiles
        let wordPoints = List.fold (fun s (w : word) -> if w.Length > 1 then s + (calculatePoints squares w) else s) 0 words
        wordLengthPoints + wordPoints
        