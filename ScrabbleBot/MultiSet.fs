// Insert your MultiSet.fs file here. All modules must be internal

module internal MultiSet

    type MultiSet<'a when 'a: comparison> = {size: uint32; map : Map<'a, uint32>}
    let empty<'a when 'a : comparison> =
        {size = 0u; map = Map.empty<'a, uint32>}


    let isEmpty (x : MultiSet<_>) = x.size = 0u

    let size (x : MultiSet<_>) = x.size

    let contains (a: 'a) (x : MultiSet<'a>) = Map.containsKey a x.map && x.map.Item a > 0u

    let numItems (a: 'a) (x : MultiSet<'a>) = if not (Map.containsKey a x.map) then 0u else x.map.Item a

    let add (a: 'a) (n:uint32) (x : MultiSet<'a>) = {size = x.size + n; map = x.map.Add (a, (numItems a x) + n)}

    let addSingle (a: 'a) (x : MultiSet<'a>) = add a 1u x

    let remove (a: 'a) (n:uint32) (x : MultiSet<'a>) =
        let toRemove = if (numItems a x) <= n then (numItems a x) else n;
        {size = (x.size - toRemove)
         map = x.map.Add (a,  (numItems a x) - toRemove)}

    let removeSingle (a: 'a) (x : MultiSet<'a>) = remove a 1u x

    let fold (f : 'a -> 'b -> uint32 -> 'a) acc (set: MultiSet<'b>) : 'a = Map.fold f acc set.map

    let foldBack (f: 'a -> uint32 -> 'b -> 'b) (set : MultiSet<'a>) (acc : 'b) : 'b = Map.foldBack f set.map acc
        