module Dictionary


type Dict =
    | Node of char * bool * Map<char,Dict>
    | StartNode of bool * Map<char,Dict>
    
let empty () = StartNode(false, Map.empty<char, Dict>)

let insert (s:string) node =
    let rec aux (cs:List<char>) (node:Dict) : Dict = 
        match cs, node with
            | [], StartNode(_, charMap) -> StartNode(true, charMap)
            | [], Node(c, _, charMap) -> Node(c, true, charMap)
            
            | c::cs, StartNode(b, charMap) ->
                let next =
                    if not (charMap.ContainsKey c) then
                        (aux cs <| Node(c, false, Map.empty<char, Dict>))
                    else
                        (aux cs <| charMap.Item c)
                StartNode(b, Map.add c next charMap)
            | c::cs, Node(ch, b, charMap) ->
                let next =
                    if not (charMap.ContainsKey c) then
                        aux cs <| Node(c, false, Map.empty<char, Dict>)
                    else
                        aux cs <| charMap.Item c
                Node(ch, b, Map.add c next charMap)
    aux <| Seq.toList s <| node
    
let lookup (s:string) (node: Dict) : bool =
    let rec aux (cs:List<char>) (node:Dict) : bool =
        match cs, node with
            | [], StartNode(b, _) -> b
            | [], Node(_, b, _) -> b
            
            | c::cs, StartNode(_, charMap) -> if not (charMap.ContainsKey c) then false else aux <| cs <| charMap.Item c
            | c::cs, Node(_, _, charMap) -> if not (charMap.ContainsKey c) then false else aux <| cs <| charMap.Item c
    aux <| Seq.toList s <| node
    
let step (c:char) (node:Dict) : (bool*Dict) option =
    let getBool = function
        | Node(_, b, _) | StartNode(b, _) -> b
    
    match node with
        | Node(_, _, charMap) | StartNode(_, charMap) -> if not (charMap.ContainsKey c) then None else Some(getBool <| (charMap.Item c), charMap.Item c)
        