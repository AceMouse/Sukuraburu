// Insert your MultiSet.fsi file here. All modules must be internal

module internal MultiSet

    type MultiSet<'a when 'a : comparison>

    val empty<'a when 'a: comparison> : MultiSet<'a>

    val isEmpty : MultiSet<'a> -> bool

    val size : MultiSet<'a> -> uint32

    val contains : 'a -> MultiSet<'a> -> bool

    val numItems : 'a -> MultiSet<'a> -> uint32

    val add : 'a -> uint32 -> MultiSet<'a> -> MultiSet<'a>

    val addSingle : 'a -> MultiSet<'a> -> MultiSet<'a>

    val remove : 'a -> uint32 -> MultiSet<'a> -> MultiSet<'a>

    val removeSingle : 'a -> MultiSet<'a> -> MultiSet<'a>

    val fold : ('a -> 'b -> uint32 -> 'a) -> 'a -> MultiSet<'b> -> 'a

    val foldBack : ('a -> uint32 -> 'b -> 'b) -> MultiSet<'a> -> 'b -> 'b
    
    val toList : MultiSet<uint32> -> Map<uint32,Set<(char*int)>> -> (uint32*Set<(char*int)>) list