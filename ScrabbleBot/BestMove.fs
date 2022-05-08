module internal BestMove

    open System.ComponentModel
    open MultiSet
    open ScrabbleUtil

    let startingSquares (placedTiles : Map<(int*int), uint32*(char*int)>) =
        let rec aux (map: Map<int*int,uint32 * (char*int)>) m offset =
            let lst = Map.toList map
            match lst with
            | [] -> m
            | ((x,y),_)::_ -> 
                aux (map.Remove ((x,y))) (
                    Set.add (x, (y+offset))     (Set.add ((x+offset), y) m)
                ) offset
        Set.toList (aux placedTiles Set.empty -1 |> fun m -> aux placedTiles m 1 |> fun m -> aux placedTiles m 0)
        
    
    let processDir coord (placedTiles : Map<(int*int), (uint32*(char*int))>) (dict : Dictionary.Dict) (hand : (uint32*Set<(char*int)>) list) d r : ((((int*int)*(uint32*(char*int))) list)*int)=
        if placedTiles.ContainsKey (((fst coord)-1), snd coord) then ([],-1000)
        else
            let used = 0uy
            let rec aux acc coord (hand : (uint32*Set<(char*int)>) list) (u:byte) (dict : Dictionary.Dict): (bool*((((int*int)*(uint32*(char*int))) list)*int)) =
                let rec tilesToExplore lst i = 
                    if (((u &&& (1uy<<<i)) = 0uy) && (i < hand.Length)) then
                        let setList = hand[i] |> snd |> Set.toList
                        let l = List.init setList.Length (fun j -> (i,((fst hand[i]),setList[j]))) @ lst
                        tilesToExplore l (i+1)
                    else if (i < hand.Length) then
                        tilesToExplore lst (i+1)
                    else
                        lst
                let toExplore = tilesToExplore [] 0
                let rec explore j =
                    let idx, defTile = toExplore[j]
                    let tile = (Option.defaultValue (defTile) (placedTiles.TryFind coord))
                    let placed = placedTiles.ContainsKey coord
                    let id,(ch,pts) = tile
                    let n = (Dictionary.step ch) dict
                    match n with
                    | None ->
                        let ret = (false,([],0)):: if j < toExplore.Length-1 then explore (j+1) else List.empty
                        ret
                    | Some (b, dict) ->
                        let x,y = coord
                        let nc = (x+d, y+r)
                        let ret = (aux (acc + pts) nc hand (if placed then u else (u ||| (1uy<<<idx))) dict)
                        let add = (b || (ret|>fst))
                        let s = ret |> snd |> fst 
                        let p = ret |> snd |> snd
                        let ret = (add,((if add && not placed then ((x,y),((id,(ch,pts))))::s else s), (if add then p + pts else p))):: if j < toExplore.Length-1 then explore (j+1) else List.empty
                        ret
                    
                List.maxBy snd (explore 0)
                        
            let ret = (aux 0 coord hand used dict) |> snd
            ret 
    
    let processDown coord (placedTiles : Map<int*int, uint32 * (char*int)>) (dict : Dictionary.Dict) (hand : (uint32 * Set<char*int>) list) : ((int*int) * (uint32 * (char*int))) list * int=
        let ret = (processDir coord placedTiles dict hand 1 0)
        ret
    let processRight coord (placedTiles : Map<int*int, uint32 * (char*int)>) (dict : Dictionary.Dict) (hand : (uint32*Set<char*int>) list) : ((int*int) * (uint32 * (char*int))) list * int=
        let ret = (processDir coord placedTiles dict hand 0 1)
        ret
            
    let rec bestSquareToMove (bestSquare:(int*int) * (((uint32 * (char*int)) list * int) * bool)) : ((int*int) * (uint32 * (char*int))) list =
        let tiles = bestSquare |> snd |> fst |> fst
        let start = bestSquare |> fst
        let dir = bestSquare |> snd |> snd
        //let pts = (bestSquare |> snd |> fst |> snd)
        let coords = List.init tiles.Length (fun i -> ( start |> fst |> (+) (if dir then i else 0), start |> snd |> (+) (if not dir then i else 0) ) )
        let lst = List.init tiles.Length (fun i -> (coords[i],tiles[i])) 
        lst
        
    let suggestMove (board : Parser.board) (placedTiles : Map<int*int, uint32 * (char*int)>) (dict : Dictionary.Dict) (hand : (uint32 * Set<char*int>) list) =
        let rec aux startingSquares =
            match startingSquares with
            | c::l -> (List.maxBy (fun t -> t |> snd) [(processDown c placedTiles dict hand); (processRight c placedTiles dict hand)])::(aux l)
            | [] -> []
        // (coord, ((longestWord, length), dir))
        // dir : Down = true, Right = false
        let ssquares = if placedTiles.IsEmpty then [board.center] else (startingSquares placedTiles) 
        let lst = aux ssquares
        let bestSquare = Seq.maxBy snd lst
        bestSquare |> fst
        
        
        
        
        
    

