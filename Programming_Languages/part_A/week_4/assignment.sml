exception NoAnswer

(* 1 - takes a string list and returns a string list that has only 
the strings in the argument that start with an uppercase letter*)

(* string list -> string list *)
fun only_capitals(xs)=
    List.filter (fn s => Char.isUpper(String.sub(s,0))) xs 

(* 2 - takes a string list and returns the longest string in the list. 
If the list is empty, return "". In the case of a tie, return the string 
closest to the beginning of the list. Use foldl, String.size, and no recursion*)

(* string list -> string *)

(*foldl f init [x1, x2, ..., xn]
returns
f(xn,...,f(x2, f(x1, init))...)
or init if the list is empty.*)

fun longest_string1 sList =
		foldl (fn (s, acc) => if (String.size s > String.size acc) then s else acc)
			  "" sList

(* 3- is exactly like longest_string1 except in the case of ties it return the 
string closest to the end of the list*)    

fun longest_string2 sList =
		foldl (fn (s, acc) => if (String.size s < String.size acc) then acc else s)
			  "" sList

(* 4- Write functions longest_string_helper, longest_string3, and longest_string4 such that:
• longest_string3 has the same behavior as longest_string1 and longest_string4 has the
same behavior as longest_string2.
• longest_string_helper has type (int * int -> bool) -> string list -> string
(notice the currying). This function will look a lot like longest_string1 and longest_string2
but is more general because it takes a function as an argument.
• If longest_string_helper is passed a function that behaves like > (so it returns true exactly
when its first argument is stricly greater than its second), then the function returned has the same
behavior as longest_string1.
• longest_string3 and longest_string4 are defined with val-bindings and partial applications
of longest_string_helper.
*)

fun longest_string_helper f sList =
		foldl (fn (s, acc) => if f(String.size s, String.size acc) then s else acc) 
			   "" sList

val longest_string3 = longest_string_helper (fn (s1, s2) => s1 > s2)
val longest_string4 = longest_string_helper (fn (s1, s2) => s1 >= s2)

(* 5 - takes a string list and returns the longest string in the list that begins with an uppercase 
letter, or "" if there are no such strings. Assume all strings have at least 1 character*)

(* string list -> string *)

val longest_capitalized = longest_string1 o only_capitals

(*6 - takes a string and returns the string that is the same characters in reverse order.
Use ML’s o operator, the library function rev for reversing lists, and two library functions
in the String module. *)

(* string -> string *)

val rev_string = implode o rev o explode

(* 7 - Write a function first_answer of type (’a -> ’b option) -> ’a list -> ’b (notice the 2 arguments are curried). 
The first argument should be applied to elements of the second argument in order
until the first time it returns SOME v for some v and then v is the result of the call to first_answer.
If the first argument returns NONE for all list elements, then first_answer should raise the exception
NoAnswer. *)

(* first_answer = fn f => SOME x  
 f acc xs *)

fun first_answer f xs =
	case xs of
		[] => raise NoAnswer
		| (x::xs') => case f x of
						NONE => first_answer f xs'
						|SOME value => value

(** 8 - Write a function all_answers of type (’a -> ’b list option) -> ’a list -> ’b list option
(notice the 2 arguments are curried). The first argument should be applied to elements of the second
argument. If it returns NONE for any element, then the result for all_answers is NONE. Else the
calls to the first argument will have produced SOME lst1, SOME lst2, ... SOME lstn and the result of
all_answers is SOME lst where lst is lst1, lst2, ..., lstn appended together (order doesn’t matter).
Hints: The sample solution is 8 lines. It uses a helper function with an accumulator and uses @. Note
all_answers f [] should evaluate to SOME [].*)

fun all_answers f xs=
	let fun helper acc xs =
		case xs of
			[] => SOME acc
			|(x::xs') => case f x of
						NONE => NONE
						| SOME value => helper (acc@value) xs'
	in helper [] xs
	end

(* Second part of the assignment *)

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


(* 9-a) Use g to define a function count_wildcards that takes a 
pattern and returns how many Wildcard patterns it contains *)

val count_wildcards = g (fn () => 1) (fn _ => 0)

(* 9-b)  Use g to define a function count_wild_and_variable_lengths that takes a pattern and returns
the number of Wildcard patterns it contains plus the sum of the string lengths of all the variables
in the variable patterns it contains.*)

val count_wild_and_variable_lengths = g (fn () => 1) String.size

(* 9- c)  Use g to define a function count_some_var that takes a string and a pattern (as a pair) and
returns the number of times the string appears as a variable in the pattern. We care only about
variable names; the constructor names are not relevant.*)

(* string * pattern -> int *)

fun count_some_var (s, p) = g (fn () => 0) (fn x => if x = s then 1 else 0) p

(* 10 - Write a function check_pat that takes a pattern and returns true if and only if all the variables
appearing in the pattern are distinct from each other (i.e., use different strings). The constructor
names are not relevant. Hints: The sample solution uses two helper functions. The first takes a
pattern and returns a list of all the strings it uses for variables. Using foldl with a function that uses
@ is useful in one case. The second takes a list of strings and decides if it has repeats. *)

(* helper 1 : pattern -> string list *)
(* helper 2 : string list -> bool *)

fun check_pat p=
	let 
		fun all_str p =
			case p of 
				Variable x => [x]
				  | TupleP ps => foldl (fn (p, i) => all_str p@i) [] ps
				  | ConstructorP (_, p)           => all_str p
				  | _                             => []

			fun no_repeat strs = 
				case strs of
					[]       => true
				  | (s::strs') => not (List.exists (fn x => s = x) strs') andalso no_repeat strs'
		in
			no_repeat (all_str p)
		end

(* 11- Write a function match that takes a valu * pattern and returns a (string * valu) list option,
namely NONE if the pattern does not match and SOME lst where lst is the list of bindings if it does.
Note that if the value matches but the pattern has no patterns of the form Variable s, then the result
is SOME []. Hints: Sample solution has one case expression with 7 branches. The branch for tuples
uses all_answers and ListPair.zip*)

fun match (v, p) =
  	case (v, p) of
      (_, Wildcard) => SOME []
    | (sv, Variable sp) => SOME [(sp,sv)]
    | (Unit, UnitP) => SOME []
    | (Const iv, ConstP ip) => if iv = ip then SOME [] else NONE
    | (Tuple tv, TupleP tp) => if List.length tv = List.length tp
                               then all_answers match (ListPair.zip(tv, tp))
                               else NONE
    | (Constructor (s1,cv), ConstructorP (s2,cp)) => if s1 = s2
                                                     then match (cv,cp)
                                                     else NONE
    | (_, _) => NONE


(* 12- Write a function first_match that takes a value and a list of patterns and returns a
(string * valu) list option, namely NONE if no pattern in the list matches or SOME lst where
lst is the list of bindings for the first pattern in the list that matches. Use first_answer and a
handle-expression.  *)

fun first_match v ps = 
		SOME (first_answer (fn p => match (v, p)) ps)
		handle NoAnswer => NONE
		
(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)