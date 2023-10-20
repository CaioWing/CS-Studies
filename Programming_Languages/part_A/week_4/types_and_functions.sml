(* higher-order functions are often polymorphic based on 
"wharever" type of function is passed but not always*)

fun times_until_zero(f,x) =
    if x = 0 then 0 else 1 + times_until_zero(f, f x)

(* f (f (f (f ... x)))*)
(* (int -> int) * int -> int *)

(*conversely, some polymorphic functions that are not higher-order*)
(* 'a list -> int *)

fun len xs =
    case xs of 
    [] => 0
    | _::xs' => 1 + len xs'