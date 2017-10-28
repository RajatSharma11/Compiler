structure MAPT = RedBlackMapFn(struct type ord_key = int val compare = Int.compare end);

structure IntSet = ListSetFn(struct type ord_key = int val compare = Int.compare end:ORD_KEY) : ORD_SET;

structure StringSet = ListSetFn( struct type ord_key = string val compare = String.compare end : ORD_KEY) : ORD_SET

type graph = (Intset.set MAPT.map  *  Intset.set MAPT.map)

fun isolate [] = []
  | isolate (x::xs) = x::isolate(List.filter (fn y => y <> x) xs);

fun strValue NONE = StringSet.empty
  | strValue x = valOf(x)

fun intValue NONE = IntSet.empty
  | intValue x = valOf(x)

fun unioni [] i set = set
|   unioni (x::xs) i set = let
			   val set = Stringset.unioni(set,strValue(MAPT.find(i,x))) 	
			   in
			       unioni xs i set
			   end

type ud = StringSet.set MAPT.map * StringSet.set MAPT.map

fun useSet i (x:ud) = strValue(MAPT.find(#1 x, i));
fun defSet i (x:ud) = strValue(MAPT.find(#2 x, i));

fun liveness (succ:Intset.set MAPT.map) inst ud (i,out) = let 
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

fun killSet [] (r:ud) = StringSet.empty
	| killSet [x] (r:ud) = strValue(MAPT.find(#2 r,x))
	| killSet (x::xs) (r:ud) = let
					val s1 = killSet(xs) r
					val s2 = strValue(MAPT.find(#2 r,x))
				in
					StringSet.union(s1,s2)
				end
fun genSet [] (r:ud) = StringSet.empty
	| genSet (x::[]) (r:ud) = strValue(MAPT.find(#1 r,x))
	| genSet (x::y::[]) (r:ud) = let
				val s1 = strValue(MAPT.find(#1 r,x))
				val s2 = StringSet.difference(strValue(MAPT.find(#1 r,y)),strValue(MAPT.find(#2 r,x)))
			in
				StringSet.union(s1,s2)
			end
	| genSet (x::y::xs) (r:ud) = let
				val s1 = genSet (y::xs) r 
				val s2 = StringSet.union(s1,strValue(MAPT.find(#1 r,x)))
				val s3 = StringSet.difference(strValue(MAPT.find(#1 r,y)),strValue(MAPT.find(#2 r,x)))
			
			in
				StringSet.union(s2,s3)
			end

fun addVertex (u,v) vertex = let
		val u = MAPT.insert(u,vertex,IntSet.empty)
		val v = MAPT.insert(v,vertex,IntSet.empty)
	in
		(u,v)
	end
fun addEdge2 l x y = let 
		val s1 = intValue(MAPT.find(l,x))
		val s2 = IntSet.add (s1,y)
		val s3 = MAPT.insert(l,x,s2)
	in
		s3
	end

fun addEdge (u,v) x y = let 
		val p = addEdge2 u y x
		val q = addEdge2 v x y 
	in
		(p,q)
	end
