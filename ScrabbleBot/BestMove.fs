module internal BestMove

    open Eval
    open Microsoft.FSharp.Collections
    open ScrabbleBot
    
    let adjSquares (placedTiles : Map<int*int, uint32 * (char*int)>)
        =
        let rec aux (map: Map<int*int,uint32 * (char*int)>) m offset =
            let lst = Map.toList map
            match lst with
            | [] -> m
            | ((x,y),_)::_ -> 
                aux (map.Remove ((x,y))) (
                    Set.add (x, (y+offset))     (
                        Set.add ((x+offset), y) (
                            Set.add (x, y) m
                        )
                    )
                ) offset
        Set.toList (aux placedTiles Set.empty -1 |> fun m -> aux placedTiles m 1)
        

    let startingSquares (adj:(int*int) list)
        =
        let rec aux (list: (int*int) list) m xoffset yoffset =
            let rec calcSquare x y (m: Map<(int*int), int>) xoffset yoffset acc : Map<(int*int), int>=
                if acc <= 8 then
                    let mx = match (m.TryFind (x,y)) with
                                | None -> Map.add (x,y) acc (calcSquare (x + xoffset) (y + yoffset) m xoffset yoffset (acc + 1) )
                                | Some value -> Map.add (x,y) (min acc value) (calcSquare (x + xoffset) (y + yoffset) m xoffset yoffset (acc + 1) )
                    calcSquare (x + xoffset) (y + yoffset) mx xoffset yoffset (acc + 1)
                else
                    m
            match list with
            | [] -> m
            | (x,y)::l -> aux l (calcSquare x y m xoffset yoffset 1) xoffset yoffset
        (Map.toList (aux adj Map.empty -1 0), Map.toList (aux adj Map.empty 0 -1))
        
        
    
    let processInDirection (coord : int*int) (placedTiles : Map<int*int, uint32 * (char*int)>)
                                 (dict : Dictionary.Dict) (legalDict : Dictionary.Dict) (hand : (uint32 * Set<char*int>) list)
                                 (adj: Set<int*int>)
                                 d r
                                 (squares : (int*int -> square))
                                 : ((int*int) * (uint32 * (char*int))) list * int
        =
        if placedTiles.ContainsKey ((fst coord) - r, (snd coord) - d)
        then ([],-1000)
        else
            let rec aux coord (hand : (uint32 * Set<char*int>) list)
                          (usedMask : byte) (dict : Dictionary.Dict)
                          (move : ((int*int) * (uint32 * (char*int))) list)
                          : (((int*int) * (uint32 * (char*int))) list * int)
                =
                let rec toBegining coord d r =
                    let x, y = coord
                    let rec aux i = 
                        match (placedTiles.ContainsKey (x+r*(i+1),y+d*(i+1))) with
                        | true -> aux (i + 1)
                        | false -> (x+r*i,y+d*i)
                    aux 0
                
                let rec illegal (ch:char) (coord:int*int) d r : bool=
                    // intentional rotation flip
                    let cx, cy = coord
                    let start = toBegining coord -r -d
                    let x, y = start
                    let rec aux i = 
                        match (placedTiles.ContainsKey (x+d*i,y+r*i)) with
                        | true -> (placedTiles.Item (x+d*i,y+r*i) |> snd |> fst |> string ) + (aux (i + 1))
                        | false -> if (x+d*i,y+r*i) = coord then (ch |> string) + (aux (i + 1)) else ""
                    
                    let word = (aux 0)
                    let ret = word.Length < 2 || (not (Dictionary.lookup word legalDict))
                    ret
                    
                let rec getWordStartingHere (m : Map<int * int,uint32 * (char * int)>) coord d r : ((int*int)*(uint32*(char*int))) list =
                    let x, y = coord
                    let rec aux i = 
                        match (Map.containsKey (x+r*i,y+d*i) m) with
                        | true -> ((x+r*i,y+d*i),(Map.find (x+r*i,y+d*i) m)) :: aux (i + 1)
                        | false -> []
                    aux 0 
                    
                    
                    
                let toExplore = Points.tilePoints usedMask hand [] 0
                let explore =
                    let folder =
                        fun (s: ((int*int) * (uint32 * (char*int))) list * int) (idx, (id,(ch,pts))) ->
                            //is there a tile placed at coord
                            let placed = placedTiles.ContainsKey coord
                            //tile is toExplore[i] unless there is already one placed
                            let id,(ch,pts) = Option.defaultValue  (id,(ch,pts)) (placedTiles.TryFind coord)
                            // are we adjacent to an already placed til
                            let adjacent = adj.Contains coord
                            //if so we check if the crossing word is legal
                            if adjacent && illegal ch coord d r then
                                //if it is not we return with a previously discovered move
                                s
                            else
                                // else we step forwards
                                let n = Dictionary.step ch dict
                                match n with
                                | None ->
                                    // if a word does not continue with ch we return a previously discovered move
                                    s
                                | Some (b, dict) ->
                                    // if a word continues with ch
                                    let x, y = coord
                                    // if ch was not already placed we place it, we make sure to add it to the current move and mark it as used in the hand
                                    let move = move @ if not placed then [(coord, (id,(ch,pts)))] else []
                                    let usedMask = if not placed then (usedMask|||(1uy<<<(int idx))) else usedMask
                                    //then we return the best move out of:
                                    // 1. the previous best
                                    // 2. the one we get if we continue the word with ch
                                    // 3. the one we get if we end on ch (only available if step resulted in a word ending on ch and there is not an already placed tile directly following ch)
                                    let lst = [s; aux (x+r,y+d) hand usedMask dict move] @
                                              if move.Length > 0 && b && (not (placedTiles.ContainsKey (x+r,y+d))) then
                                                  let m = List.fold (fun s (coord,tile) -> Map.add coord tile s) placedTiles move
                                                  let words = [getWordStartingHere m (move.Item 0 |> fst) d r]
                                                  // intentional rotation shift as we are looking at crossing words
                                                  let words = List.fold (fun words (coord,_) -> (getWordStartingHere m (toBegining coord -r -d) r d) :: words) words move
                                                  [(move, Points.getMovePoints squares move words)]
                                              else
                                                  []
                                    List.maxBy snd lst 
                            
                    List.fold folder ([], 0) toExplore
                    
                explore
                
            aux coord hand 0uy dict []
    
    let processDown coord (placedTiles : Map<int*int, uint32 * (char*int)>)
                          (dict : Dictionary.Dict)
                          (legaldict : Dictionary.Dict)
                          (hand : (uint32 * Set<char*int>) list)
                          (adj: Set<int*int>)
                          (squares : (int*int -> square))
                          : ((int*int) * (uint32 * (char*int))) list * int
        = processInDirection coord placedTiles dict legaldict hand adj 1 0 squares
        
    let processRight coord (placedTiles : Map<int*int, uint32 * (char*int)>)
                          (dict : Dictionary.Dict )
                          (legaldict : Dictionary.Dict)
                          (hand : (uint32 * Set<char*int>) list)
                          (adj: Set<int*int>)
                          (squares : (int*int -> square))
                          : ((int*int) * (uint32 * (char*int))) list * int
        =
            let x,y = coord
            processInDirection coord placedTiles dict legaldict hand adj 0 1 squares
        
    let suggestMove (board : Parser.board)
                    (placedTiles : Map<int*int, uint32 * (char*int)>)
                    (dicts : Map<int,Dictionary.Dict>)
                    (hand : (uint32 * Set<char*int>) list)
        =
        let adj = adjSquares placedTiles
        let adjSet = Set.ofList adj
        let squares = fun c -> if Map.containsKey c placedTiles
                                then board.defaultSquare
                                else match board.squares c with
                                      | StateMonad.Result.Success v -> v.Value
                                      | StateMonad.Result.Failure _ -> failwith "Failed to get square."
        
        let rec aux (startSquares: ((int*int)*int) list) (down : bool)  =
            Array.Parallel.map
                (
                 fun (coord, minlen) ->
                    if down then
                        processDown coord placedTiles (dicts.Item (max minlen 2)) (dicts.Item 2) hand adjSet squares
                    else
                        processRight coord placedTiles (dicts.Item (max minlen 2)) (dicts.Item 2) hand adjSet squares
                )
                (List.toArray startSquares)  
            
        // (coord, ((longestWord, length), dir))
        // dir : Down = true, Right = false
        
        let right, down = if placedTiles.IsEmpty then startingSquares [board.center] else (startingSquares adj)
        
        let rlst = aux right false
        let dlst = aux down true
        
        // return best square
        fst (Seq.maxBy snd (Array.toList dlst @ Array.toList rlst))
        