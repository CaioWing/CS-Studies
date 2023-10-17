datatype exp = Constant of int
            | Negate of int
            | Add of exp * exp
            | Multiply of exp * exp

fun eval (Constant i) = if
    | eval (Negate e2) = ~ eval e2
    | eval (Add (e1, e2)) = eval e1 + eval e2
    | eval (Multiply (e1, e2)) = eval e1 * eval e2

