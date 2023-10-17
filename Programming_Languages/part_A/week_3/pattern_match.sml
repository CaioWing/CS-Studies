(* Bad style *)

fun full_name (r : {first: string, middle : string, last: string}) =
    case r of 
        {first = x, middle = y, last = z} => x ^ " " ^ y ^ " " ^ z

fun sum_tiple1(triple: int * int * int) =
    case triple of 
        (x, y, z) => x + y + z

(* Good style *)

fun full_name2 (r : {first: string, middle : string, last: string}) = 
    let val {first = x, middle = y, last = z} = r
    in 
        x ^ " " ^ y ^ " " ^ z
    end

fun sum_triple2 triple =
    let val (x, y, z) = triple
    in 
        x + y + z
    end

(*The simple one Style *)

fun full_name3 {first = x, middle = y, last = z} =
    x ^ " " ^ y ^ " " ^ z

fun sum_triple3 (x, y, z) =
    x + y = z