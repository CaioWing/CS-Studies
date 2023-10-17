(* type is int list -> int *)
fun sum_list xs =
    case xs of
        [] => 0
        | x :: xs' => x + sum_list xs'

(* type is 'a list * 'a list -> 'a list *)
fun append (xs, ys) =
    case xs of
        [] => ys
        | x :: xs' => x :: append(xs', ys)

datatype 'a option = NONE | SOME of 'a

(* similarly, here are polymorphic lists but without special syntax*)

datatype 'a mylist = Empty | Cons of 'a * 'a mylist

(*a fanciar example for binary trees where internal nodes have data 
of any 1 type and leaves have data of another possibly-different type*)

datatype ('a, 'b) tree = Node of 'a * ('a, 'b) tree * ('a, 'b) tree
                        | Leaf of 'b

(*type is (int, int) tree -> int *)
fun sum_tree tr =
    case tr of
        Leaf i => i
        | Node(i, lft, rgt) => i + sum_tree lft + sum_tree rgt

(*type is ('a, int) tree -> int *)
fun sum_leaves tr =
    case tr of
        Leaf i => i
        | Node(i, lft, rgt) => sum_leaves lft + sum_leaves rgt


datatype ('a,'b) flower =
         Node of ('a,'b) flower * ('a,'b) flower
         | Leaf of 'a
         | Petal of 'b