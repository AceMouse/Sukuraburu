module internal BestMove

    open System.Threading.Tasks
    open Microsoft.FSharp.Collections
    open ScrabbleBot
    open Microsoft.FSharp.Linq
    
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
                                 (infinite : bool)
                                 : ((int*int) * (uint32 * (char*int))) list * int
        =
        if placedTiles.ContainsKey ((fst coord) - r, (snd coord) - d)
        then ([],-1000)
        else
            let used = 0uy
            let rec aux (acc:int) coord (hand : (uint32 * Set<char*int>) list)
                                  (usedMask : byte) (dict : Dictionary.Dict)
                                  : bool * (((int*int) * (uint32 * (char*int))) list * int)
                =
                let rec toBegining coord d r =
                    let x, y = coord
                    let rec aux i = 
                        match (placedTiles.ContainsKey (x+r*i,y+d*i)) with
                        | true -> aux (i + 1)
                        | false -> (x+r*i,y+d*i)
                    aux 0
                
                let rec illegal (ch:char) (coord:int*int) d r : bool=
                    // intentional rotation flip
                    let start = toBegining coord -r -d
                    let x, y = start
                    let rec aux i = 
                        match (placedTiles.ContainsKey (x+d*i,y+r*i)) with
                        | true -> (placedTiles.Item (x+d*i,y+r*i) |> snd |> fst |> string ) + (aux (i + 1))
                        | false -> if (x+d*i,y+r*i) = coord then (ch |> string) + (aux (i + 1)) else ""
                    let word = (aux 0)
                    word.Length < 2 || not (Dictionary.lookup word legalDict)
                let outOfBounds (x,y) =
                     (not infinite) && (x > 7 || x < -7 || y > 7 || y < -7)  
                    
                let toExplore = Points.tilePoints usedMask hand [] 0
                let rec explore j =
                    (*if outOfBounds coord then
                        [(false,([], 0 ))]
                    else*)
                    if (j >= toExplore.Length) then
                        [(false,([], -1000 ))]
                    else 
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
                                let ret = (aux (acc + pts) nc hand (if placed then usedMask else (usedMask ||| (1uy<<<idx))) dict)
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
                          (infinite: bool)
                          : ((int*int) * (uint32 * (char*int))) list * int
        = processInDirection coord placedTiles dicts[minLen] dict hand adj 1 0 infinite
        
    let processRight coord minLen (placedTiles : Map<int*int, uint32 * (char*int)>)
                          (dicts : Dictionary.Dict list)
                          (dict : Dictionary.Dict)
                          (hand : (uint32 * Set<char*int>) list)
                          (adj: Set<int*int>)
                          (infinite : bool)
                          : ((int*int) * (uint32 * (char*int))) list * int
        = processInDirection coord placedTiles dicts[minLen] dict hand adj 0 1 infinite
        
    let suggestMove (board : Parser.board) (placedTiles : Map<int*int, uint32 * (char*int)>)
                    (dicts : Dictionary.Dict list) (hand : (uint32 * Set<char*int>) list)
                    (infinite: bool)
        =
        let adj = adjSquares placedTiles
        let adjSet = Set.ofList adj
        
        let rec aux (startSquares: ((int*int)*int) list) (down : bool)  =
            Array.Parallel.map
                (
                 fun (coord, minlen) ->
                    if down then
                        processDown coord minlen placedTiles dicts dicts[0] hand adjSet infinite
                    else
                        processRight coord minlen placedTiles dicts dicts[0] hand adjSet infinite
                )
                (List.toArray startSquares)  
            
        // (coord, ((longestWord, length), dir))
        // dir : Down = true, Right = false
        
        let right, down = if placedTiles.IsEmpty then startingSquares [board.center] else (startingSquares adj)
        let arr = Array2D.create 16 16 -1
        let rightlen =  (List.length right) - 1
        for i in 0.. rightlen do
            let (x, y), len = right.Item i
            let inbounds x = x <= 7 && x >= -8 
            if inbounds x && inbounds y then
                arr[y+8,x+8] <- len*10
        
        for y in 0.. 15 do
            for x in 0.. 15 do
                System.Console.Write arr[y,x]
                System.Console.Write " "
            System.Console.Write "\n"
            
        let arr = Array2D.create 16 16 -1
        let downlen =  (List.length down) - 1
        for i in 0.. downlen do
            let (x, y), len = down.Item i
            let inbounds x = x <= 7 && x >= -8 
            if inbounds x && inbounds y then
                arr[y+8,x+8] <- len*10
        System.Console.WriteLine "\n----------------"
        for y in 0.. 15 do
            for x in 0.. 15 do
                System.Console.Write arr[y,x]
                System.Console.Write " "
            System.Console.Write "\n"
            
            
        let rlst = aux right false
        let dlst = aux down true
        
        // return best square
        fst (Seq.maxBy snd (Array.toList dlst @ Array.toList rlst))
        