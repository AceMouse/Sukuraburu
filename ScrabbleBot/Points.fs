
module ScrabbleBot.Points
    // a -> DL
    // b -> TL
    // c -> DW
    // d -> TW
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
    //let calculate = z
    
    let getMovePoints move placedTiles : int=
        List.length move         