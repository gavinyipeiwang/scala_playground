
(* 1 *)
fun is_older(d1: int * int * int, d2: int * int * int) = 
	#1 d1 < #1 d2 
		orelse (#1 d1 = #1 d2 andalso #2 d1 < #2 d2) 
			orelse (#1 d1 = #1 d2 andalso #2 d1 = #2 d2 andalso #3 d1 < #3 d2)

(* 2 *)
fun number_in_month(xs: (int * int * int) list, mth: int) = 
	let
		fun count(acc: int, xs: (int * int * int) list) = 
			if null xs then acc
			else if #2 (hd xs) = mth then count(acc + 1, tl xs) 
					 else count(acc, tl xs)
	in
		count(0, xs)
	end

(* 3 *)
fun number_in_months(xs: (int * int * int) list, mths: int list) = 
	if null xs orelse null mths 
	then 0 
	else number_in_month(xs, hd mths) + number_in_months(xs, tl mths)

(* 4 *)
fun dates_in_month(xs: (int * int * int) list, mth: int) =
	if null xs then []
	else if #2 (hd xs) = mth then hd(xs) :: dates_in_month(tl xs, mth)
			 else dates_in_month(tl xs, mth)

(* 5 *)
fun dates_in_months(xs: (int * int * int) list, mths: int list) = 
	if null mths then []
	else dates_in_month(xs, hd mths) @ dates_in_months(xs, tl mths)

(* 6 *)
fun get_nth(xs: string list, nth: int) = 
	let
		fun take(xs: string list, i: int) = 
			if i = nth
			then hd xs
			else take(tl xs, i + 1)
	in
		take(xs, 1)
	end

(* 7 *)
fun date_to_string(date: int * int * int) = 
	let
		val mthsStrs = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
		val mthStr = get_nth(mthsStrs, #2 date)
		val dayStr = Int.toString (#3 date)
		val yearStr = Int.toString (#1 date)
	in
		mthStr^" "^dayStr^", "^yearStr
	end

(* 8 *)
exception NumberBeforeReachingSum
fun number_before_reaching_sum(sum: int, xs: int list) = 
	let
		fun count(acc: int, xs: int list, i: int) =
			if null xs then if acc >= sum then i
											else raise NumberBeforeReachingSum
			else if acc + hd(xs) >= sum then i
			     else count(acc + hd(xs), tl xs, i + 1)
	in
		if null xs then 0
		else count(0, xs, 0)
	end

(* 9 *)
fun what_month(day: int) = 
	let
		val mthsDays = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	in
		number_before_reaching_sum(day, mthsDays) + 1
	end

(* 10 *)
fun month_range(day1: int, day2: int) = 
	if day1 > day2
  then []
  else what_month(day1) :: month_range(day1 + 1, day2)

(* 11 *)
fun oldest(xs: (int * int * int) list) = 
	if null xs then NONE
	else 
			let
				val tl_oldest = oldest(tl xs)
			in
				if isSome tl_oldest andalso is_older(valOf(tl_oldest), hd xs) then tl_oldest
				else SOME (hd xs) 
			end

(* 12 *)
fun isIn(x: int, lst: int list) = 
	if null lst then false
	else x = hd(lst) orelse isIn(x, tl lst)

(* It's not the best implementation of removing duplicates from a given list. *)
fun removeDuplicates(xs: int list) = 
	let
		fun helper(acc: int list, xs: int list) = 
			if null xs
			then acc
			else if isIn(hd xs, acc) then helper(acc, tl xs)
				   else helper(acc @ [hd(xs)], tl xs)
	in
		helper([], xs)
	end

fun number_in_months_challenge(xs: (int * int * int) list, mths: int list) = 
	number_in_months(xs, removeDuplicates(mths))

fun dates_in_months_challenge(xs: (int * int * int) list, mths: int list) = 
	dates_in_months(xs, removeDuplicates(mths))

fun is_leapYear(year: int) = ((year mod 400) = 0 orelse (year mod 4) = 0) andalso not ((year mod 100) = 0)

fun reasonable_date(date: int * int * int) =
	let
		val year = #1 date
		val month = #2 date
		val day = #3 date
	in
		if not(year > 0) orelse not(month > 0 andalso month < 13) orelse not(day > 0) then false
		else if is_leapYear(year) andalso month = 2 then day <= 29
				 else if month = 4 orelse month = 6 orelse month = 9 orelse month = 11 then day <= 30
				 	    else if month = 2 then day <= 28
				 	    else day <= 31
	end


