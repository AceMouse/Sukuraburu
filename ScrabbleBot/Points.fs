module internal ScrabbleBot.Points
    open Eval
    open StateMonad

    let rec tilePoints (usedMask:byte) (hand : (uint32 * Set<char*int>) list) lst i =
        if (((usedMask &&& (1uy<<<i)) = 0uy) && (i < hand.Length))
        then
            let setList = hand[i] |> snd |> Set.toList
            // uncomment to use wildcards correctly :D it's slow...
            let l = List.init 1(*setList.Length*) (fun j -> (i,((fst hand[i]),setList[j]))) @ lst
            tilePoints usedMask hand l (i+1)
        else if (i < hand.Length)
        then
            tilePoints usedMask hand lst (i+1)
        else
            lst
  
    let getWord (word : word) =
        (List.fold (fun s (c : char * int) -> (c |> fst) :: s) [] word) |> List.toArray |> System.String
    
    let getMoveWord (word : ((int*int)*(uint32*(char*int))) list) =
        (List.foldBack (fun (c : (int*int)*(uint32*(char*int))) s -> (c |> snd |> snd |> fst) :: s) word []) |> List.toArray |> System.String

              
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
        
    let getMovePoints (squares : int*int -> square option) (move : ((int*int)*(uint32*(char*int))) list) (words : ((int*int)*(uint32*(char*int))) list list) : int = 
        let containsHole = List.fold (fun s tile -> (squares (fst tile)).IsNone || s) false move
        if containsHole then -1000
        else
            let wordLengthPoints = if move.Length = 7 then 50 else 0
            let wordPoints = List.fold (fun s (w : ((int*int)*(uint32*(char*int))) list) ->
                                if w.Length > 1
                                then s + (calculatePoints // Calculate points for current word
                                              (List.map (fun (coord, _) -> (squares coord).Value) w) // Get squares at the tiles coordinates
                                              (List.fold (fun s wrd -> (wrd |> snd |> snd) :: s) // Get list of tiles (the word)
                                                [] w)
                                         )
                                else s
                                ) 0 words
            wordLengthPoints + wordPoints
        