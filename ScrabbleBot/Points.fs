
module ScrabbleBot.Points
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
    let calculate : ((int * int) * (uint32 * (char * int))) list = 
    
    let getMovePoints (move:((int*int)*(uint32*(char*int))) list) placedTiles : int=
        (List.sumBy (fun (_,(_,(_,p))) -> p) move) + if move.Length = 7 then 50 else 0
        