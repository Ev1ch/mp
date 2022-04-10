type date = int * int * int;




fun is_older(a: date, b: date) = 
  if (#1 b) > (#1 a)
  then true
  else

  if  (#2 b) > (#2 a)
  then true
  else
  
  if (#3 b) > (#3 a)
  then true
  else
  
  false;

fun is_older_test() =
  if is_older((1, 1, 1), (2, 1, 1)) <> true
  then raise Fail "Test failed at 1 case."
  else

  if is_older((1, 1, 1), (1, 2, 1)) <> true
  then raise Fail "Test failed at 2 case."
  else

  if is_older((1, 1, 1), (1, 1, 2)) <> true
  then raise Fail "Test failed at 3 case."
  else
  
  if is_older((1, 1, 1), (1, 1, 1)) <> false
  then raise Fail "Test failed at 4 case."
  else

  print("Test of is_older passed.");


(* Helper *)
fun is_date_in_month(date: date, month: int) =
  if #2 date = month
  then true
  else false;

fun number_in_month(dates: date list, month: int) =
  if null dates
  then 0
  else
  
  if is_date_in_month((hd dates), month)
  then 1 + number_in_month(tl dates, month)
  else number_in_month(tl dates, month);

fun number_in_month_test() =
  if number_in_month([(1, 1, 1), (2, 1, 1)], 1) <> 2
  then raise Fail "Test failed at 1 case."
  else

  if number_in_month([], 1) <> 0
  then raise Fail "Test failed at 2 case."
  else

  if number_in_month([(1, 2, 1)], 1) <> 0
  then raise Fail "Test failed at 3 case."
  else

  print("Test of number_in_month passed.");


(* Helper *)
fun is_date_in_months(date: date, months: int list) = 
  if null months
  then false
  else
  
  if is_date_in_month(date, hd months)
  then true
  else is_date_in_months(date, tl months);

fun number_in_months(dates: date list, months: int list) = 
  if null dates
  then 0
  else
  
  if is_date_in_months(hd dates, months)
  then 1 + number_in_months(tl dates, months)
  else number_in_months(tl dates, months);

fun number_in_months_test() =
  if number_in_months([(1, 1, 1), (1, 2, 1)], [1, 2]) <> 2
  then raise Fail "Test failed at 1 case."
  else

  if number_in_months([(1, 1, 1), (1, 2, 1)], [3, 4]) <> 0
  then raise Fail "Test failed at 2 case."
  else

  if number_in_months([], [1]) <> 0
  then raise Fail "Test failed at 3 case."
  else

  if number_in_months([(1, 2, 1)], [1]) <> 0
  then raise Fail "Test failed at 4 case."
  else

  print("Test of number_in_months passed.");


fun dates_in_month(dates: date list, month: int) =
  if null dates
  then []
  else
  
  if #2 (hd dates) = month
  then (hd dates) :: dates_in_month(tl dates, month)
  else dates_in_month(tl dates, month);

fun dates_in_month_test() =
  if dates_in_month([(1, 1, 1), (2, 1, 1)], 1) <> [(1, 1, 1), (2, 1, 1)]
  then raise Fail "Test failed at 1 case."
  else

  if dates_in_month([(1, 2, 1), (2, 2, 1)], 1) <> []
  then raise Fail "Test failed at 2 case."
  else

  if dates_in_month([(1, 1, 1), (1, 2, 1)], 1) <> [(1, 1, 1)]
  then raise Fail "Test failed at 3 case."
  else

  print("Test of dates_in_month passed.");


fun dates_in_months(dates: date list, months: int list) =
  if null dates
  then []
  else
  
  if is_date_in_months(hd dates, months)
  then (hd dates) :: dates_in_months(tl dates, months)
  else dates_in_months(tl dates, months);

fun dates_in_months_test() =
  if dates_in_months([(1, 1, 1), (1, 2, 1)], [1, 2]) <> [(1, 1, 1), (1, 2, 1)]
  then raise Fail "Test failed at 1 case."
  else

  if dates_in_months([(1, 1, 1), (1, 2, 1)], [3]) <> []
  then raise Fail "Test failed at 2 case."
  else

  if dates_in_months([], [1]) <> []
  then raise Fail "Test failed at 3 case."
  else

  print("Test of dates_in_months passed.");


fun get_nth(list: 'a list, n: int) =
  if n = 1
  then hd list
  else get_nth(tl list, n - 1);

fun get_nth_test() =
  if get_nth([1, 2, 3], 1) <> 1
  then raise Fail "Test failed at 1 case."
  else 

  if get_nth([1, 2, 3], 2) <> 2
  then raise Fail "Test failed at 2 case."
  else

  print("Test of get_nth passed.");


(* Helper *)
fun month_to_string(month: int) =
  let
    val months = [
      "January", 
      "February", 
      "March", 
      "April", 
      "May", 
      "June", 
      "July", 
      "August", 
      "September", 
      "October", 
      "November", 
      "December"
    ]
  in
    get_nth(months, month)
  end;

fun date_to_string(date: date) = 
  month_to_string(#2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date);

fun date_to_string_test() =
  if date_to_string((1, 1, 1)) <> "January 1, 1"
  then raise Fail "Test failed at 1 case."
  else 

  if date_to_string((2, 2, 2)) <> "February 2, 2"
  then raise Fail "Test failed at 2 case."
  else

  print("Test of date_to_string passed.");


fun number_before_reaching_sum(numbers: int list, sum: int) =
  if null numbers
  then 0
  else

  if (hd numbers) < sum
  then 1 + number_before_reaching_sum(tl numbers, sum - (hd numbers))
  else 0;

fun number_before_reaching_sum_test() =
  if number_before_reaching_sum([1, 2, 3], 1) <> 0
  then raise Fail "Test failed at 1 case."
  else 

  if number_before_reaching_sum([1, 2, 3], 2) <> 1
  then raise Fail "Test failed at 2 case."
  else

  if number_before_reaching_sum([1, 2, 3], 3) <> 1
  then raise Fail "Test failed at 3 case."
  else

  if number_before_reaching_sum([1, 2, 3], 4) <> 2
  then raise Fail "Test failed at 4 case."
  else

  print("Test of number_before_reaching_sum passed.");


fun what_month(day: int) =
  let
    val months = [
      31,
      28,
      31,
      30,
      31,
      30,
      31,
      31,
      30,
      31,
      30,
      31
    ]
  in
    number_before_reaching_sum(months, day) + 1
  end;

fun what_month_test() =
  if what_month(1) <> 1
  then raise Fail "Test failed at 1 case."
  else 

  if what_month(31) <> 1
  then raise Fail "Test failed at 2 case."
  else

  if what_month(32) <> 2
  then raise Fail "Test failed at 3 case."
  else

  if what_month(365) <> 12
  then raise Fail "Test failed at 4 case."
  else

  print("Test of what_month passed.");


(* Helper *)
fun numbers_range(min: int, max: int) =
  if min <= max
  then min :: numbers_range(min + 1, max)
  else [];

fun months_range(a: int, b: int) =
  if a > b
  then []
  else numbers_range(
    what_month(a),
    what_month(b)
  );

fun months_range_test() =
  if months_range(2, 1) <> []
  then raise Fail "Test failed at 1 case."
  else 

  if months_range(1, 31) <> [1]
  then raise Fail "Test failed at 2 case."
  else

  if months_range(1, 32) <> [1, 2]
  then raise Fail "Test failed at 3 case."
  else

  if months_range(1, 365) <> [
    1, 
    2, 
    3, 
    4, 
    5, 
    6, 
    7, 
    8, 
    9, 
    10, 
    11, 
    12
  ]
  then raise Fail "Test failed at 4 case."
  else

  print("Test of months_range passed.");


(* Helper *)
fun oldest_search(dates: date list, oldest_date: date) =
  if null dates 
  then oldest_date
  else

  if is_older(oldest_date, hd dates)
  then oldest_search(tl dates, oldest_date)
  else oldest_search(tl dates, hd dates)

fun oldest(dates: date list) =
  if null dates
  then NONE
  else

  SOME(
    oldest_search(tl dates, hd dates)
  )

fun oldest_test() =
  if oldest([(1, 1, 1), (1, 2, 1)]) <> SOME((1, 1, 1))
  then raise Fail "Test failed at 1 case."
  else

  if oldest([(1, 1, 1), (1, 1, 1)]) <> SOME((1, 1, 1))
  then raise Fail "Test failed at 2 case."
  else

  if oldest([]) <> NONE
  then raise Fail "Test failed at 3 case."
  else

  print("Test of oldest passed.");




is_older_test();
number_in_month_test();
number_in_months_test();
dates_in_month_test();
dates_in_months_test();
get_nth_test();
date_to_string_test();
number_before_reaching_sum_test();
what_month_test();
months_range_test();
oldest_test();