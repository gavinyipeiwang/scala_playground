(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* a *)

fun all_except_option(str, strl) = 
	let
		fun helper(lst, acc) = 
			case lst of
				[] => NONE
			| x :: xs' => if same_string(str, x) then SOME (acc @ xs')
				            else helper(xs', acc @ [x])
	in
		helper(strl, [])
	end

(* b *)
fun get_substitutions1(strll, str) = 
	case strll of
		[] => []
	| x :: xs' => case all_except_option(str, x) of
									NONE => get_substitutions1(xs', str)
								| SOME strl => strl @ get_substitutions1(xs', str)

(* c *)
fun get_substitutions2(strll, str) = 
	let
		fun helper(lst, acc) =
		 	case lst of
		 		[] => acc
		 	| x :: xs' => case all_except_option(str, x) of
		 									NONE => helper(xs', acc)
		 								| SOME strl => helper(xs', acc @ strl)
	in
		helper(strll, [])
	end

(* d *)
fun similar_names(strll, {first=x, middle=y, last=z}) =
	let
		fun helper(lst, acc) = 
			case lst of
				[] => acc
			| first_name :: xs' => helper(xs', acc @ [{first=first_name, last=z, middle=y}])
	in
		helper(get_substitutions2(strll, x), [{first=x, last=z, middle=y}])
	end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* a *)
fun card_color (Clubs, _) = Black
	| card_color (Spades, _) = Black
	| card_color (Diamonds, _) = Red
	| card_color (Hearts, _) = Red

(* b *)
fun card_value (_, Num(x)) = x
	| card_value (_, Ace) = 11
	| card_value (_, _) = 10

(* c *)
fun remove_card(cs, c, e) =
	case cs of
		[] => raise e
	| x :: xs' => if x = c then xs'
	              else x :: remove_card(xs', c, e)

(* d *)
fun all_same_color cs = 
	let
		fun helper(lst, colour) = 
			case lst of
				[] => true
			| x :: [] => card_color(x) = colour
			| h1 :: h2 :: xs' => if card_color(h1) = card_color(h2) then helper(xs', colour)
													 else false		
	in
		case cs of
			[] => true
		| c :: [] => true
		| x :: xs' => helper(xs', card_color(x))
	end

(* e *)
fun sum_cards cs = 
	let
		fun helper(lst, acc) = 
			case lst of
				[] => acc
			| x :: xs' => helper(xs', card_value(x) + acc)
	in
		helper(cs, 0)
	end

(* f *)
fun score(cs, goal) =
	let
		val sum = sum_cards(cs)
		val preliminary_score = if sum_cards(cs) > goal then 3 * (sum - goal)
														else goal - sum
	in
		if all_same_color cs then preliminary_score div 2
		else preliminary_score
	end

(* g *)
fun officiate(cs, moves, goal) = 
	let
		fun helper(held, left_moves, left_cards) = 
			case left_moves of
				[] => score(held, goal)
			| Discard(c) :: xs' => helper(remove_card(held, c, IllegalMove), xs', left_cards)
			| Draw :: ys' =>  case left_cards of
											 		[] => score(held, goal)
											  | z :: zs' => if (card_value(z) + sum_cards(held)) > goal then score(z :: held, goal) 
											 	              else helper(z :: held, ys', zs')

	in
		helper([], moves, cs)
	end

