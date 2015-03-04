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
val only_capitals =
	List.filter (fn str => 
		Char.isUpper(String.sub(str, 0)))

fun longest_string1 str_list =
	foldl (fn (s1, s2) => if (String.size s1) > (String.size s2) then s1 else s2) "" str_list

fun longest_string2 str_list =
	foldl (fn (s1, s2) => if (String.size s2) > (String.size s1) then s2 else s1) "" str_list

fun longest_string_helper f str_list =
	foldl (fn (s1, s2) => if f(String.size s1, String.size s2) then s1 else s2) "" str_list

val longest_string3 =
	longest_string_helper (fn (a, b) => a > b)

val longest_string4 =
	longest_string_helper (fn (a, b) => a >= b)

val longest_capitalized =
	longest_string1 o only_capitals

val rev_string = 
	String.implode o rev o String.explode

fun first_answer f alist = 
	case alist of
		[] => raise NoAnswer
		|x::xs => case (f x) of
					SOME a => a
					|NONE => first_answer f xs

fun all_answers f alist =
	let
		fun helper alist acc =
			case alist of
				[] => SOME acc
				|x::xs => case (f x) of
							NONE => NONE
							|SOME a => helper xs (a::acc)
	in
		helper alist []
	end