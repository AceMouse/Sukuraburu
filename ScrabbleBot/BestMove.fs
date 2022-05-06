module internal BestMove

    open MultiSet
    open ScrabbleUtil

    let startingSquares (placedTiles : Map<(int*int), uint32*(char*int)>) =
        let rec aux (lst: (((int*int)*(uint32*(char*int))) list)) m offset =
            match lst with
            | [] -> m
            | ((x,y),_)::l -> 
                aux l (Set.add (x, (y+offset))(Set.add ((x+offset), y) m)) offset
        Set.toList (aux (Map.toList placedTiles) (aux (Map.toList placedTiles) (aux (Map.toList placedTiles) (aux (Map.toList placedTiles) (aux (Map.toList placedTiles) (aux (Map.toList placedTiles) (aux (Map.toList placedTiles) (aux (Map.toList placedTiles) (aux (Map.toList placedTiles) (aux (Map.toList placedTiles) Set.empty 0) 1) -1) -2) -3) -4) -5) -6) -7) -8)
        
    
    let processDir coord (placedTiles : Map<(int*int), (uint32*(char*int))>) (dict : Dictionary.Dict) (hand : (uint32*Set<(char*int)>) list) d r : (string*int)=
        if placedTiles.ContainsKey (((fst coord)-1), snd coord) then ("",-1000)
        else
            let used = 0uy
            let rec aux curAcc (lastWordAcc:int) coord (hand : (uint32*Set<(char*int)>) list) (u:byte) i (dict : Dictionary.Dict): (bool*(string*int)) =
                if (((u &&& (1uy<<<i)) = 0uy) && (i < hand.Length)) then
                    let setList = hand[i] |> snd |> Set.toList
                    let toExplore = List.init setList.Length (fun i -> ((fst hand[i]),setList[i]))
                    let rec explore j =
                        let tile = (Option.defaultValue (toExplore[j]) (placedTiles.TryFind coord))
                        let ch = tile |> snd |> fst
                        let pts = tile |> snd |> snd
                        let n = (Dictionary.step ch) dict
                        match n with
                        | None ->
                            let ret = (false,("",lastWordAcc)):: if j < toExplore.Length-1 then explore (j+1) else List.empty
                            ret
                        | Some (b, dict) ->
                            let ret = List.maxBy snd [(aux (curAcc+pts) (if b then curAcc + pts else lastWordAcc) (coord |> fst |> (+)d, coord |> snd |> (+)r) hand (u ||| (1uy<<<i)) 0 dict); (aux curAcc lastWordAcc coord hand u (i+1) dict)]
                            let add = (b || (ret|>fst))
                            let s = ret |> snd |> fst 
                            let p = ret |> snd |> snd
                            (add,((if add then (ch|>string) + s else s), p)):: if j < toExplore.Length-1 then explore (j+1) else List.empty
                        
                    List.maxBy snd (explore 0)
                        
                else
                    (false,("",lastWordAcc))
            (aux 0 0 coord hand used 0 dict) |> snd
    
    let processD coord (placedTiles : Map<(int*int), (uint32*(char*int))>) (dict : Dictionary.Dict) (hand : (uint32*Set<(char*int)>) list) : (string*int)*bool=
        (processDir coord placedTiles dict hand 1 0,true)
    let processR coord (placedTiles : Map<(int*int), (uint32*(char*int))>) (dict : Dictionary.Dict) (hand : (uint32*Set<(char*int)>) list) : (string*int)*bool=
        (processDir coord placedTiles dict hand 0 1,false)
            
    let rec bestSquareToMove (bestSquare:(int*int)*((string*int)*bool)) : ((int*int)*(uint32*(char*int))) list =
        let chars = bestSquare |> snd |> fst |> fst |> Seq.toList
        let start = bestSquare |> fst
        let dir = bestSquare |> snd |> snd
        //let pts = (bestSquare |> snd |> fst |> snd)
        let coords = List.init chars.Length (fun i -> ( start |> fst |> (+) (if dir then i else 0), start |> snd |> (+) (if not dir then i else 0) ) )
        let lst = List.init chars.Length (fun i -> (coords[i], (((((chars[i]|> int)-('a'|> int))+1) |> uint32),(chars[i],0))))
        lst
        
    let suggestMove (board : Parser.board) (placedTiles : Map<(int*int), (uint32*(char*int))>) (dict : Dictionary.Dict) (hand : (uint32*Set<(char*int)>) list) =
        let rec aux startingSquares =
            match startingSquares with
            | c::l -> (c,(List.maxBy (fun t -> t |> fst |> snd) [(processD c placedTiles dict hand); (processR c placedTiles dict hand)]))::(aux l)
            | [] -> []
        // (coord, ((longestWord, length), dir))
        // dir : Down = true, Right = false 
         
        let bestSquare = (Seq.maxBy snd (aux (if placedTiles.IsEmpty then ([board.center]) else (startingSquares placedTiles))))
        
        bestSquareToMove bestSquare
        
        
        
        
        
    

