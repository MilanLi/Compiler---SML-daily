(*---------------------------------------------------*)

(*Practice Exercises 1*)

(*---------------------------------------------------*)

(*1-1*)
fun min (x, y) = 
	if x < y 
	then x
	else y;


(*1-2*)
fun fib 1 = 1
	|fib 2 = 1
	|fib n = fib(n-1) + fib(n-2);


(*1-3*)
fun isPrime x = if(x <= 1) then false 
	else
		let fun checkPrime n = 
			if n = 1 then true 
			else 
				if x = n
				then checkPrime(n-1)
				else
					if x mod n = 0
					then false
					else checkPrime(n-1)
				in
					checkPrime x
				end;


(*1-4*)
fun sumList [] = 0
	|sumList (a::l) = a + sumList l;


(*1-5*)
fun squareList [] = []
	|squareList (a::l) = (a*a)::squareList l;

(*---------------------------------------------------*)

(*Practice Exercises 2*)

(*---------------------------------------------------*)

(*2-1*)
datatype expr = 
	NUM of int 
	| PLUS of expr * expr
	| MINUS of expr * expr
	| TIMES of expr * expr
	| DIV of expr * expr
	| F of expr list * (int list -> int);

fun eval x = case x of
	NUM x => x
	| PLUS (y, z) => eval y + eval z
	| MINUS (y,z) => eval y - eval z
	| TIMES (y,z) => eval y * eval z
	| DIV (y, z) => eval y div eval z
	| F (l, f) => f (map eval l);


(*2-2*)
(*approach 1*)
fun flatten (a::l) = foldl (op @) a l;

(*approach 2*)
fun flatten xs =
	let fun f ([], y) = y
			|f (x, y) = foldr (op ::) y x
			in		
				foldr f [] xs
			end
			;


(*2-3*)
fun map f [] = []
	|map f l = foldr (fn (x, y) => (f x):: y) [] l


(*2-4*)
fun filter g [] = []
	|filter g l = foldr (fn (x, y) => if(g x) then x::y else y) [] l;


(*2-5*)
fun count g [] = 0
	|count g l = foldl (fn(x, y) => if g x then y+1 else y) 0 l;


(*2-6*)
fun mapPartial opt [] = []
	|mapPartial opt l = 
	let
		fun f (x, y) = 
			case opt x of
				NONE => y
				| SOME x => x::y
	in
		foldr f [] l
	end

(*---------------------------------------------------*)

(*Practice Exercises 3*)

(*---------------------------------------------------*)

(*3-1*)
functor F(M: ORD_MAP where type Key.ord_key = string)
		 (S: ORD_SET where type Key.ord_key = string) :> 
sig
	val proc: string list -> S.set M.map
end
= 
struct
	fun proc stringList = 
		let
			(*val myMap = M.empty*)
		 	fun parseTextFile([], myMap) = myMap
		 		| parseTextFile(infile::l, myMap) = 
		 		let
		 			val ins = TextIO.openIn infile
		 			val wd = ref "";
		 			fun helper(copt: char option, myMap) =
		 				case copt of
		 					NONE => ((*(print((!wd)^"\n");*)
		 							TextIO.closeIn ins;
		 							myMap
		 							)
		 					|SOME(c) => 
		 						if Char.isSpace(c) 
		 						then( 
		 							(*print((!wd)^"\n");*)
		 							let
		 								val temp = !wd
		 								val () = wd := ""
		 							in
		 								case (M.find(myMap, temp)) of
	 							 		NONE => (
	 							 				print(temp^" added a word\n");
	 							 				helper(TextIO.input1 ins, M.insert(myMap, temp, S.add(S.empty, infile)))
	 							 				)
	 							 				(*print(Int.toString(M.numItems(myMap))) )*)
		 							 		
		 							 	| SOME(s) => ( 
		 							 				print(temp^" changed a word\n");
		 							 				helper(TextIO.input1 ins, M.insert(myMap, temp, S.add(s, infile)))
		 							 				)
		 							end
		 							) 
		 						else	(
		 								wd := ((!wd)^Char.toString(c));
		 								helper(TextIO.input1 ins, myMap)
		 								)
		 								(*print(Char.toString(c)); *)
		 		in
		 			(
		 			parseTextFile(l, helper(TextIO.input1 ins, myMap))
		 			)
		 		end
		in
		 	parseTextFile (stringList, M.empty)
		end 
end

structure Strset = SplaySetFn(struct
								type ord_key = string
								val compare = String.compare
							end)

structure Strmap = SplayMapFn(struct
								type ord_key = string
								val compare = String.compare
							end)

structure Init = F(Strmap)(Strset);
val resultMap = Init.proc ["a.txt", "b.txt"];
