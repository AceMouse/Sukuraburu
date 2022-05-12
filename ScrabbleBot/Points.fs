module ScrabbleBot.Points

    // a -> DL
    // b -> TL
    // c -> DW
    // d -> TW
    
    let rec tilePoints (usedMask:byte) (hand : (uint32 * Set<char*int>) list) lst i = 
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