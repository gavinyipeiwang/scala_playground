(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

(* 1 *)
fun only_capitals strs =
	List.filter(fn x => Char.isUpper(String.sub(x,0))) strs

(* 2 *)
fun longest_string1 strs =
	List.foldl(fn (x, acc) => if String.size x > String.size acc then x else acc) "" strs

(* 3 *)
fun longest_string2 strs = 
	List.foldl(fn (x, acc) => if String.size x >= String.size acc then x else acc) "" strs

(* 4 *)
fun longest_string_helper f strs = 
	List.foldl(fn (acc, x) => if f(String.size acc, String.size x) then acc else x) "" strs

val longest_string3 = longest_string_helper(fn (a, b) => a > b)

val longest_string4 = longest_string_helper(fn (a, b) => a >= b)

(* 5 *)
fun longest_capitalized strs =
	(longest_string1 o only_capitals) strs

(* 6 *)
fun rev_string str = 
	(String.implode o List.rev o String.explode) str

(* 7 *)
fun first_answer f [] = raise NoAnswer
  | first_answer f (x :: xs) =
			case f x of
				NONE   => first_answer f xs
	    | SOME s => s

(* 8 *)
fun all_answers f xs =
  let
   fun helper([], acc) = SOME acc
    | helper(x :: xs, acc) =
        case f x of
          NONE   => NONE
        | SOME s => helper(xs, s @ acc)
  in
    helper(xs, [])
  end

(* 9 *)
val count_wildcards = g(fn _ => 1)(fn _ => 0)
val count_wild_and_variable_lengths = g(fn _ => 1) String.size
fun count_some_var(s, p) = g(fn _ => 0)(fn x => if x = s then 1 else 0) p

(* 10 *)
fun check_pat patt =
  let
    fun helper1(Variable xs) = [xs]
      | helper1(TupleP ps) = List.foldl(fn (x, acc) => helper1(x) @ acc) [] ps
      | helper1(ConstructorP(s, v)) = helper1 v
      | helper1(_) = []
    fun helper2([]) = true
      | helper2(x :: xs) = not (List.exists(fn y => x = y) xs) andalso helper2 xs
  in
    (helper2 o helper1) patt
  end

(* 11 *)
fun match(_, Wildcard) = SOME []
  | match(v, Variable s) = SOME [(s, v)]
  | match(Unit, UnitP) = SOME []
  | match(Const a, ConstP b) = if a =b then SOME [ ] else NONE
  | match(Tuple ps, TupleP qs) =
      if length ps = length qs then all_answers match (ListPair.zip(ps, qs))
      else NONE
  | match(Constructor(s, v), ConstructorP(t, w)) =
      if s = t then match(v, w)
      else NONE
  | match _ = NONE

(* 12 *)
fun first_match v ps =
	SOME (first_answer (fn x => match(v, x)) ps) handle NoAnswer => NONE
