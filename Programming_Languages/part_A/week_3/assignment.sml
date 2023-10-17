(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(*1-a) Takes a string and a string list. Return NONE if the string is not in the
 list, else return SOME lst where lst is identical to the argument list except 
 the string is not in it*)

fun all_except_option(s1  : string, list_s2 : string list) =
    let fun helper(strings : string list) =
            case strings of
                [] => []
                | hstr::tstr => if s1 <> hstr
                                then hstr :: helper(tstr)
                                else helper(tstr)
    in  
        if list_s2 = helper(list_s2)
        then NONE 
        else SOME (helper(list_s2))
    end

(*1-b) takes a string list list and a string s and returns a string list. The result 
has all the strings that are in some list in substitutions that also has s, but s 
itself should not be in the result*)

fun get_substitutions1(list_subs : string list list, s1 : string) =
    case list_subs of
        [] => []
        | head::tail => let val aux = get_substitutions1(tail, s1)
                        in
                            case all_except_option(s1, head) of
                                NONE => aux
                                | SOME list_str => list_str @ aux
                        end


(* 1-c) write a helper function get_substitutions1, which is like get_substitutions1
 except it uses a tail-recursive local helper function*)

fun get_substitutions2(list_subs : string list list, s1 : string) =
    let fun helper(str_List)=
            case str_List of   
                [] => []
                | head::tail => let val aux = get_substitutions1(tail, s1)
                        in
                            case all_except_option(s1, head) of
                                NONE => helper(tail)
                                | SOME list_str => list_str @ helper(tail)
                        end
    in
        helper(list_subs)
    end

(* 1-d) takes a string list list of substitutions and a full name of type {first:string,
 middle:string, last:string} list and return a list of full names ({first:string, middle:string, 
 last:string} list). The result is all the full names you can produce by substituting for the 
 first name using substitutions. The answer should begin with the original name*)

fun similar_names(xss, {first=f, middle=m, last=l}) = 
    let 
      val other_firsts = f::get_substitutions1(xss, f)
    in
      let
      fun get_answer(sub_firsts, names) =
        case sub_firsts of
          [] => names
        | x::xs' => get_answer(xs', names@[{first = x, middle=m, last=l}])
      in
      get_answer(other_firsts, [])
    end
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

(*2-a) takes a card and returns its value (spades and clubs are black,
 diamonds and hearts are red)*)

 fun card_color(Card : card)=
    case Card of
        (suit, rank) => if suit = Spades orelse suit = Clubs 
                        then Black 
                        else Red

(*2-b) takes a card and returns its value (numbered cards 
have their number as the value, aces are 11, everyhing else is 10)*)

(* (suit * rank) -> Int *)

fun card_value(c : card)=
    case c of 
        (suit, rank) => if rank = King orelse rank = Queen orelse rank = Jack 
                        then 10
                        else if rank = Ace
                        then 11
                        else 
                            case rank of
                                Num value => value
                                | _ => raise IllegalMove
                                


(*2-c) takes a list of cards cs, a card c, and and exception e. It returns a 
list that has the elements of cs except c. If the c is in the list more than once,
 remove only the first one. If c not in the list, raise the exception e*)

(* Card list * Card * Exception ->  Card list *)


fun remove_card(cs : card list, c : card, e)=
    let fun helper(cs : card list, remove : bool)=
            case cs of
            [] => []
            | head::tail => if head = c
                            then
                                if remove = true
                                then helper(tail, false)
                                else head:: helper(tail, remove)
                            else head :: helper(tail, remove)
    in
        helper(cs, true)
    end


(*2-d) takes a list of cards and returns true if all the cards in the list are 
the same color.*)

(* card list -> bool *)

fun all_same_color(cs : card list)=
    case cs of  
    [] => true
    | _::[] => true
    | head::neck::rest => card_color head = card_color neck andalso all_same_color(neck :: rest)

(*2-e) takes a list of cards and returns the sum of their values*)

(* card list -> int *)

fun sum_cards(cs : card list)=
    case  cs of
    [] => 0
    | head::tail => card_value head + sum_cards(tail)

(*2-f) takes a card list (the held-cards) and an int (the goal) and computes the scrore*)

(*A game is played with a card-list and a goal. The player has a list of held-cards, initially empty. The player
makes a move by either drawing, which means removing the first card in the card-list from the card-list and
adding it to the held-cards, or discarding, which means choosing one of the held-cards to remove. The game
ends either when the player chooses to make no more moves or when the sum of the values of the held-cards
is greater than the goal.
The objective is to end the game with a low score (0 is best). Scoring works as follows: Let sum be the sum
of the values of the held-cards. If sum is greater than goal, the preliminary score is three times (sum−goal),
else the preliminary score is (goal − sum). The score is the preliminary score unless all the held-cards are
the same color, in which case the score is the preliminary score divided by 2 (and rounded down as usual
with integer division; use ML’s div operator).*)

(* card list, int -> int *)
fun score(cs : card list, goal : int)=
    let fun result cs=
        if sum_cards cs > goal 
        then 3*(sum_cards cs - goal)
        else goal - sum_cards cs
    in
        if all_same_color cs 
        then result cs div 2
        else result cs
    end

(* takes a card list a move list, and an int (the goal) and returns the score at the end of the game after processing
 the moves in the move list in order*)

 (* card list, move list, int -> int *)

 fun officiate(cs : card list, moves : move list, goal : int) = 
    let fun game(cs, ms, hs) = 
            case ms of
            [] => score(hs, goal)
            | m::ms => case m of
                        Discard d => game(cs, ms, remove_card(hs, d, IllegalMove))
                        | _ => case cs of
                                   [] => score(hs, goal)
                                   |c::cs => if sum_cards(hs) > goal
                                             then score(c::hs, goal)
                                             else game(cs, ms, c::hs)
    in
    game(cs, moves, [])
    end