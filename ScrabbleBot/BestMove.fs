module internal BestMove

    open Microsoft.FSharp.Collections
    
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
                                 : ((int*int) * (uint32 * (char*int))) list * int
        =
        if placedTiles.ContainsKey ((fst coord) - r, (snd coord) - d)
        then ([],-1000)
        else
            let used = 0uy
            let rec aux (acc:int) coord (hand : (uint32 * Set<char*int>) list)
                                  (u : byte) (dict : Dictionary.Dict)
                                  : bool * (((int*int) * (uint32 * (char*int))) list * int)
                =
                let rec toBegining coord d r =
                    let x, y = coord
                    let nx = x+r
                    let ny = y+d
                    if placedTiles.ContainsKey (nx,ny) then
                        toBegining (nx,ny) d r
                    else
                        (x,y)
                
                let rec illegal (ch:char) (coord:int*int) d r : bool=
                    let start = toBegining coord -d -r
                    let rec aux (c:int*int) :string =
                        let x, y = c
                        let cha = 
                            if c = coord then Some(ch)
                            else
                                match placedTiles.TryFind coord with
                                | None -> None
                                | Some (_,(a, _)) -> Some(a)
                        match cha with
                        | None -> ""
                        | Some value -> (value|> string) + (aux (x+r, y+d)) 
                    not (Dictionary.lookup (aux start) legalDict)
                    
                let rec tilesToExplore lst i = 
                    if (((u &&& (1uy<<<i)) = 0uy) && (i < hand.Length))
                    then
                        let setList = hand[i] |> snd |> Set.toList
                        let l = List.init setList.Length (fun j -> (i,((fst hand[i]),setList[j]))) @ lst
                        tilesToExplore l (i+1)
                    else if (i < hand.Length)
                    then
                        tilesToExplore lst (i+1)
                    else
                        lst
                let toExplore = tilesToExplore [] 0
                let rec explore j =
                    let idx, defTile = toExplore[j]
                    let tile = (Option.defaultValue defTile (placedTiles.TryFind coord))
                    let placed = placedTiles.ContainsKey coord
                    let id,(ch,pts) = tile
                    if adj.Contains coord && illegal ch coord d r then (false,([],if placed then -1000 else 0)):: if j < toExplore.Length-1 then explore (j+1) else List.empty
                    else
                        let n = (Dictionary.step ch) dict
                        match n with
                        | None ->
                            let ret = (false,([],if placed then -1000 else 0)):: if j < toExplore.Length-1 then explore (j+1) else List.empty
                            ret
                        | Some (b, dict) ->
                            let x,y = coord
                            let nc = (x+r, y+d)
                            let ret = (aux (acc + pts) nc hand (if placed then u else (u ||| (1uy<<<idx))) dict)
                            let add = (b || (ret|>fst))
                            let s = ret |> snd |> fst 
                            let p = ret |> snd |> snd
                            let ret = (add,((if add && not placed then ((x,y),(id,(ch,pts)))::s else s), (if add then p + pts else p))):: if j < toExplore.Length-1 then explore (j+1) else List.empty
                            ret
                    
                List.maxBy snd (explore 0)
                        
            let ret = (aux 0 coord hand used dict) |> snd
            ret 
    
    let processDown coord minLen (placedTiles : Map<int*int, uint32 * (char*int)>)
                          (dicts : Dictionary.Dict list)
                          (dict : Dictionary.Dict)
                          (hand : (uint32 * Set<char*int>) list)
                          (adj: Set<int*int>)
                          : ((int*int) * (uint32 * (char*int))) list * int
        = processInDirection coord placedTiles dicts[minLen] dict hand adj 1 0
        
    let processRight coord minLen (placedTiles : Map<int*int, uint32 * (char*int)>)
                          (dicts : Dictionary.Dict list)
                          (dict : Dictionary.Dict)
                          (hand : (uint32 * Set<char*int>) list)
                          (adj: Set<int*int>)
                          : ((int*int) * (uint32 * (char*int))) list * int
        = processInDirection coord placedTiles dicts[minLen] dict hand adj 0 1
        
    let suggestMove (board : Parser.board) (placedTiles : Map<int*int, uint32 * (char*int)>)
                    (dicts : Dictionary.Dict list) (hand : (uint32 * Set<char*int>) list)
        =
        let adj = adjSquares placedTiles
        let adjSet = Set.ofList adj
        let rec aux (startingSquares: ((int*int)*int) list) (down : bool)  =
            match startingSquares with
                | (coord,minlen)::l -> (if down then (processDown coord minlen placedTiles dicts dicts[0] hand adjSet) else (processRight coord minlen placedTiles dicts dicts[0] hand adjSet)) :: (aux l down) 
                | [] -> []
            
        // (coord, ((longestWord, length), dir))
        // dir : Down = true, Right = false
        
        let right, down = if placedTiles.IsEmpty then startingSquares [board.center] else (startingSquares adj)
        let rlst = aux right false
        let dlst = aux down true
        
        // return best square
        fst (Seq.maxBy snd (dlst @ rlst))
        