structure MAPT = RedBlackMapFn(struct type ord_key = int val compare = Int.compare end);

structure IntSet = ListSetFn(struct type ord_key = int val compare = Int.compare end:ORD_KEY) : ORD_SET;

structure StringSet = ListSetFn( struct type ord_key = string val compare = String.compare end : ORD_KEY) : ORD_SET

type graph = (Intset.set keyMap.map  *  Intset.set keyMap.map)

fun isolate [] = []
  | isolate (x::xs) = x::isolate(List.filter (fn y => y <> x) xs);

fun strValue NONE = Stringset.empty
  | strValue x = valOf(x)

fun intValue NONE = Intset.empty
  | intValue x = valOf(x)

fun liveness (succ:Intset.set keyMap.map)
