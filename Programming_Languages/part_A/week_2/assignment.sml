
(*
Author: Caio Wingeter
linkedIn: https://www.linkedin.com/in/caio-wingeter-165034174/
*)


(*aux functions*)

fun data2days(date : int * int * int) = 
    #1 date*365 + #2 date*30 + #3 date

(*Exercise 1: takes two dates and evaluates to true or false. It evaluates to true
if the first argument is a date that comes begore the second argument

(int * int * int), (int * int* int) -> boolean *)

fun is_older(date1 : int * int * int, date2 : int * int * int) =
    if (data2days date1 < data2days date2)
    then true
    else false


(*Exercise 2: takes a list of dates and a month and returns how many dates in the
 list are in the given month
 
 ((int * int * int) list, int -> int*)

fun number_in_month(dates : (int * int * int) list, month : int) =
    if null dates
    then 0
    else if #2 (hd dates) = month
    then 1 + number_in_month(tl dates, month)
    else number_in_month(tl dates, month)


(*Exercise 3: takes a list of dates and a list of months and returns the number
 of dates in the list of dates that are in any of the months in the list of months*)

 (*(int * int * int) list, list int -> int*)
 
 fun number_in_months(dates : (int * int * int) list, months : int list) = 
    if null months 
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)

(*Exercise 4: takes a list of dates and a month and returns a list holding the dates 
from the argument list of dates that are in the month. The list should contain dates 
in the order they were originally given*)

 (*(int * int * int) list, int -> int list*)

fun dates_in_month(dates : (int * int * int) list, month : int) =
    if null dates
    then []
    else if #2 (hd dates) = month
    then (hd dates) :: dates_in_month(tl dates, month)
    else dates_in_month(tl dates, month)

(*Exercise 5: takes a list of dates and a list of months and returns a list holding the 
dates from the argument list of dates that are in any of the months in the list of months. 
Assume the list of months has no number repeated*)

 (*(int * int * int) list, int list -> (int * int * int) list*)

fun dates_in_months(dates : (int * int * int) list, months : int list) =
    if null months
    then []
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

(*Exercise 6: takes a list of strings and an int n and return the n^th element of the list 
where the head of the list is 1st.*)

(*string list, int -> (int * int * int) list*)

fun get_nth(words : string list, n : int) =
    if n = 1
    then hd words
    else get_nth(tl words, n - 1)

(*Exercise 7: takes a date and returns a string of the form January 20, 2013. For producing 
the month part, do not use a bunch of conditionals.*)

(*(int * int * int) list -> string list*)

fun date_to_string(date : (int * int * int)) =
    let val months = ["January", "February", "March", "April", 
    "May", "June", "July", "August", "September", "October", "November", "December"]

    in 
        get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end

(*Exercise 8: takes an int called sum, which you can assume is positive, and an int list, which 
you can assume contains all positive numbers, and return an int. You should return an int n such 
that the first n elements of the list add to less than sum, but the first n+1 elements of the list 
add to sum or more. Assume the entire list sums to more than the passed in value*)

(*int, int list -> int*)

fun number_before_reaching_sum(sum : int, numbers : int list) =

    if sum - hd numbers > 0
    then 1 + number_before_reaching_sum(sum - hd numbers, tl numbers)
    else 0

(*Exercise 9: takes a day of year (between 1 and 365) and returns what month that day is in*)

(*int -> string*)

fun what_month(day : int) =
    let val days_month = 31
    in
        if (day mod days_month) = 0
        then day div days_month 
        else (day div days_month) + 1
    end

(*Exercise 10: takes two days of the year, day1 and day2 and returns an int list [m1, m2, ..., mn] 
where m1 is the month of day1, m2 is the month of day2, ..., and mn is the month of day day2. Note 
the result will have length day2 - day1 + 1 or length 0 if day1 > day2*)

(* int, int -> int list*)

fun month_range(day1 : int, day2 : int) =
    if day1 > day2
    then []
    else 
        let fun count_month(from : int, to : int) =
                if from = to
                then what_month(to)::[]
                else what_month(from)::count_month(from + 1, to) 
        in count_month(day1, day2)
        end

(*Exercise 11: takes a list of dates and evaluates to an (int*int*int) option. 
It evaluates to NONE if the list has no dates and SOME d if the date d is the 
oldest date in the list*)

(* (int*int*int) list -> (int*int*int) option*)

fun oldest(dates : (int*int*int) list) =
    if null dates
    then NONE
    else 
        let val tl_ans = oldest(tl dates)
        in 
            if isSome tl_ans andalso data2days(valOf tl_ans) < data2days(hd dates)
            then tl_ans
            else SOME (hd dates)
        end

(*TODO: challenge problems*)