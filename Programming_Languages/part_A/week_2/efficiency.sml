(*Section 1: let expressions to avioid repreated computation*)

(*badly named: evaluates to 0 on empty list*)
fun bad_max(xs : int list) = 
    if null xs 
    then 0
    else if null (tl xs)
    then hd xs
    else if hd xs > bad_max(tl xs)
    then hd xs
    else bad_max(tl xs)

    (*up here the function is called two times per 
    stage when the max element is not in the head
    if the list -> Exponential time complexity 
    (O(n^2))*)

fun good_max(xs : int list) =
    if null xs
    then 0 (*horrible style*)
    else if null (tl xs)
    then hd xs
    else
        let val tl_ans = good_max(tl xs)
        in 
            if hd xs > tl_ans
            then hd xs
            else tl_ans
        end