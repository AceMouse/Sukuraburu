module internal BestMove

    open MultiSet
    open ScrabbleUtil

    let startingSquares (placedTiles : Map<(int*int), (char*int)>) =
        let rec aux (lst: (((int*int)*(char*int)) list)) m offset =
            match lst with
            | [] -> m
            | ((x,y),_)::l -> 
                aux l (Set.add (x, (y+offset))(Set.add ((x+offset), y) m)) offset
        Set.toList (aux (Map.toList placedTiles) (aux (Map.toList placedTiles) (aux (Map.toList placedTiles) (aux (Map.toList placedTiles) (aux (Map.toList placedTiles) (aux (Map.toList placedTiles) (aux (Map.toList placedTiles) (aux (Map.toList placedTiles) (aux (Map.toList placedTiles) (aux (Map.toList placedTiles) Set.empty 0) 1) -1) -2) -3) -4) -5) -6) -7) -8)
        
    
    let processDir coord (placedTiles : Map<(int*int), (char*int)>) (dict : Dictionary.Dict) (hand : MultiSet<int>) d r : (string*int)=
        if placedTiles.ContainsKey (((fst coord)-1), snd coord) then ("",-1000)
        else
            let lst = toList hand
            let used = 0uy
            let rec aux coord (l : int list) (u:byte) i (dict : Dictionary.Dict): (string*int) =
                if (((u &&& (1uy<<<i)) = 0uy) && (i < l.Length)) then
                    let step = fst (Option.defaultValue (((('a'|>int)-1 + lst[i])|> char),0) (placedTiles.TryFind coord)) 
                    let n = (Dictionary.step step) dict
                    match n with
                    | None | Some (false, _) -> ("",-1000)
                    | Some (true, dict) ->
                        let m = List.maxBy snd [(aux (coord |> fst |> (+)d, coord |> snd |> (+)r) l (u ||| (1uy<<<i)) 0 dict); (aux coord l u (i+1) dict)]
                        (m |> fst, m |> snd |> (+)1)
                else
                    ("",0)
            aux coord lst used 0 dict
    
    let processD coord (placedTiles : Map<(int*int), (char*int)>) (dict : Dictionary.Dict) (hand : MultiSet<int>) : (string*int)*bool=
        (processDir coord placedTiles dict hand 1 0,true)
    let processR coord (placedTiles : Map<(int*int), (char*int)>) (dict : Dictionary.Dict) (hand : MultiSet<int>) : (string*int)*bool=
        (processDir coord placedTiles dict hand 0 1,false)
            
    let rec bestSquareToMove (bestSquare:(int*int)*((string*int)*bool)) : ((int*int)*(uint32*(char*int))) list =
        let chars = bestSquare |> snd |> fst |> fst |> Seq.toList
        let start = bestSquare |> fst
        let dir = bestSquare |> snd |> snd
        let len = (bestSquare |> snd |> fst |> snd)
        let coords = List.init len (fun i -> ( start |> fst |> (+) (if dir then i else 0), start |> snd |> (+) (if not dir then i else 0) ) )
        let lst = List.init len (fun i -> (coords[i], (((((chars[i]|> int)-('a'|> int))+1) |> uint32),(chars[i],0))))
        lst
        
    let suggestMove (board : Parser.board) (placedTiles : Map<(int*int), (char*int)>) (dict : Dictionary.Dict) (hand : MultiSet<int>) =
        let rec aux startingSquares =
            match startingSquares with
            | c::l -> (c,(List.maxBy (fun t -> t |> fst |> snd) [(processD c placedTiles dict hand); (processR c placedTiles dict hand)]))::(aux l)
            | [] -> []
        // (coord, ((longestWord, length), dir))
        // dir : Down = true, Right = false 
        let bestSquare = (Seq.maxBy snd (aux (startingSquares placedTiles)))
        bestSquareToMove bestSquare
        
        
        
        
        
    

