(* 1 *) val x = 1
            (* x maps to 1*)
(* 2 *) val f y = x + y
            (* x maps to a fuction that adds 1 to its argument *)
(* 3 *) val x = 2
            (* x maps to 2*)
(* 4 *) val y = 3
            (* y maps to 3*)
(* 5 *) val z = f (x +  y)
            (* z maps to 6*)