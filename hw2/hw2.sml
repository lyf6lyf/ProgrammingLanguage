(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option(str, str_list) =
	case str_list of
		[] => NONE
		|s::xs => 
			if same_string(str, s) then SOME xs
			else case all_except_option(str, xs) of
					NONE => NONE
					|SOME e => SOME (s::e)
(*	let
		fun helper(str_list, acc) =
			case str_list of
			[] => NONE
			|s::xs => if same_string(str, s) then SOME (acc @ xs) else helper(xs, s::acc)
	in
		helper(str_list, [])
	end*)

fun get_substitutions1(str_list_list, str) =
	case str_list_list of
		[] => []
		|s::xs => case all_except_option(str, s) of 
					NONE => get_substitutions1(xs, str)
					|SOME res => res @ get_substitutions1(xs, str)

fun get_substitutions2(str_list_list, str) =
	let 
		fun helper(str_list_list, acc) =
			case str_list_list of
				[] => acc
				|s::xs => case all_except_option(str, s) of
						NONE => helper(xs, acc)
						|SOME res => helper(xs, acc @ res)
	in
		helper(str_list_list, [])
	end

fun similar_names(str_list_list, {first=f, middle=m, last=l}) =
	let 
		(*fun in_list(str, str_list) =
			case str_list of
				[] => false
				|x::xs => if same_string(x, str) then true else in_list(str, xs)
		fun remove_dumplicates(str_list, acc) =
			case str_list of
				[] => acc
				|s::xs => if in_list(s, acc) then remove_dumplicates(xs, acc) else remove_dumplicates(xs, s::acc)*)
		fun get_result str_list =
			case str_list of
				[] => []
				|s::xs => {first=s, middle=m, last=l}::get_result(xs)
	in
		get_result(f::get_substitutions1(str_list_list, f))
	end 
			
		

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color(card_suit, card_rank)=
	case card_suit of
		Clubs => Black
		|Spades => Black
		|_ => Red

fun card_value(card_suit, card_rank)=
	case card_rank of
		Num i => i
		|Ace => 11
		|_ => 10

fun remove_card(card_list, c, e) =
	case card_list of
		[] => raise e
		|x::xs => if x=c then xs else x::remove_card(xs, c, e)

fun all_same_color(card_list) =
	case card_list of
		_::[] => true
		|c1::c2::rest => card_color(c1)=card_color(c2) andalso all_same_color(c2::rest)

fun sum_cards cards =
	let 
		fun helper(cards, sum) =
			case cards of
				[] => sum
				|c::rest => helper(rest, sum + card_value(c))
	in
		helper(cards, 0)
	end

fun score(cards, goal) =
	let
		val sum = sum_cards(cards)
		val score = if sum>goal then 3 * (sum-goal) else goal-sum
	in
		if all_same_color(cards)
		then score div 2
		else score
	end

fun officiate(cards, moves, goal) =
	let 
		fun helper(cards, moves, helds) =
			case moves of
				[] => score(helds, goal)
				|move::rest => (
					case move of
						Discard card => helper(cards, rest, remove_card(helds, card, IllegalMove))
						|Draw => (
							case cards of 
								[] => score(helds, goal)
								|card::xs => if sum_cards(card::helds) >= goal
												then score(card::helds, goal)
												else helper(xs, rest, card::helds)))
	in
		helper(cards, moves, [])
	end

