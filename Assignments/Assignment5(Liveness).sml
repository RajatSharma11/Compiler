structure MAPT = RedBlackMapFn(struct type ord_key = int val compare = Int.compare end);

structure IntSet = ListSetFn(struct type ord_key = int val compare = Int.compare end:ORD_KEY) : ORD_SET;

structure StringSet = ListSetFn( struct type ord_key = string val compare = String.compare end : ORD_KEY) : ORD_SET

type graph = (Intset.set keyMap.map  *  Intset.set keyMap.map)

fun isolate [] = []
  | isolate (x::xs) = x::isolate(List.filter (fn y => y <> x) xs);

fun strValue NONE = StringSet.empty
  | strValue x = valOf(x)

fun intValue NONE = IntSet.empty
  | intValue x = valOf(x)

fun unioni [] i set = set
|   unioni (x::xs) i set = let
			   val set = Stringset.unioni(set,vstr(keyMap.find(i,x))) 	
			   in
			       unioni xs i set
			   end

type ud = StringSet.set MAPT.map * StringSet.set MAPT.map

fun useSet i (x:ud) = strValue(MAPT.find(#1 x, i));
fun defSet i (x:ud) = strValue(MAPT.find(#2 x, i));

fun liveness (succ:Intset.set keyMap.map) inst ud (i,out) = let 
val x = StringSet.union(useSet inst ud, StringSet.difference(strValue(MAPT.find(out,inst)),defSet inst ud))
val y = unioni (IntSet.listItems(strValue(MAPT.find(succ,inst)))) i (strValue(MAPT.find(out,inst)))
val w = StringSet.equal(x,strValue(MAPT.find(i,inst)))
val z = StringSet.equal(y,strValue(MAPT.find(out,inst)))
in
if(w andalso z) then
(MAPT.insert(i,inst,x),MAPT.insert(out,inst,y),0)
else
(MAPT.insert(i,inst,x),MAPT.insert(out,inst,y),1)
end
