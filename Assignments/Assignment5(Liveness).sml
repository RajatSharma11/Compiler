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
fun add1 d x y = 	(*From x to y *)
	let 
		val s1 = strValue(MAPT.find(d,x))
		val s2 = StringSet.add (s1,y)
		val d = MAPT.insert(d,x,s2)
	in
		d
	end

fun add2 d vertex = 
	let
		val d = MAPT.insert(d,vertex,StringSet.empty)
		
	in
		d
	end

val graph = (MAPT.empty,MAPT.empty)
val graph = addVertex graph 1
val graph = addVertex graph 2
val graph = addVertex graph 3
val graph = addVertex graph 4

val graph = addEdge graph 1 2
val graph = addEdge graph 2 3
val graph = addEdge graph 2 4
val inst=[1,2,3,4]
val u = MAPT.empty
val d = MAPT.empty
val u = add2 u 1
val u = add2 u 2
val u = add2 u 3
val u = add2 u 4

val u = add1 u 1 "c"
val u = add1 u 1 "d"
val u = add1 u 2 "a"
val u = add1 u 2 "d"
val u = add1 u 3 "a"
val u = add1 u 3 "d"
val u = add1 u 4 "b"
val u = add1 u 4 "c"

val d = add2 d 1
val d = add2 d 2
val d = add2 d 3
val d = add2 d 4

val d = add1 d 1 "b"
val d = add1 d 2 "b"
val d = add1 d 3 "a"
val d = add1 d 4 "d"
val ud = (u,d)

fun fix_point2  (pred,succ) [] ud (i,o) update  = ((i,o),update)
| fix_point2 (pred,succ) (x::xs) ud (i,o) update  = let
						val io = liveness succ x ud (i,o)
						val change = #3 io
						in
							fix_point2 (pred,succ) (xs) ud (#1 io,#2 io) (update + change)
						end
								
fun fix_point g inst ud (i,o) update  = if(update=0) then
						(i,o)
				     else
				     		let 
				     			val io = fix_point2 g inst ud (i,o) 0 
				     			val change = #2 io
				     		in
				     			fix_point g inst ud (#1 io) change
				     		end

val i_o = fix_point graph inst ud (MAPT.empty,MAPT.empty) 1

fun mGraph [] graph inst = graph
| mGraph (x::xs) graph inst = mGraph xs (MAPT.insert(graph,x,inst)) inst 

fun cGraph [] r (u,d) graph lst inst = ((u,d),graph,lst)
| cGraph (x::xs) r (u,d) graph lst inst = let
						val u2 = genSet x r
						val d2 = killSet x r
						val graph = mGraph x graph inst
						
					in
						cGraph  xs r (MAPT.insert(u,inst,u2),MAPT.insert(d,inst,d2)) graph (inst::lst)(inst+1)			
					end

val cg = cGraph [[1,2],[3],[4]] ud (MAPT.empty,MAPT.empty) (MAPT.empty) [] 1
fun emptyGraph [] g = g
| emptyGraph (x::xs) g = emptyGraph xs (MAPT.insert(g,x,IntSet.empty))
