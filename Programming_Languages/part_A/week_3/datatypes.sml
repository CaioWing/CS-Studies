(*Datatype binding*)

datatype mytype = TwoInts of int*int
                | Str of string
                | Pizza

(* mytype  -> int *)
fun f (x : mytype) = 
    case x of 
        Pizza => 3
    |   Str s => 8
    |   TwoInts (i1, i2) => i1 + i2

(*Expression Trees*)

datatype exp = Constant of int
            | Negate of exp
            | Add of exp * exp 
            | Multiply of exp * exp

fun eval e =
    case e of
        Constant i => i
        | Negate e2 => ~ (eval e2)
        | Add(e1, e2) => (eval e1) + (eval e2)
        | Multiply(e1, e2) => (eval e1) * (eval e2)

fun max_constant e = 
    let fun max_of_two(e1, e2) = 
            let val m1 = max_constant e1
                val m2 = max_constant e2
            in if m1 > m2 then m1 else m2 end
    in 
    case e of 
        Constant i => i
        | Negate e2 => max_constant e2
        | Add(e1, e2) => max_of_two (e1, e2)
        | Multiply(e1, e2) => max_of_two (e1, e2)
    end

val example_exp : exp = Add (Constant 19, Negate (Constant 4))
val example_ans : int = eval example_exp
val fifteen = max_constant( Add (Constant 15, Negate (Constant 4)))