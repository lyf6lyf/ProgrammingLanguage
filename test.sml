fun is_older (d1: int*int*int, d2: int*int*int) =
	if #1 d1 < #1 d2 then true
	else if #1 d1 > #1 d2 then false 
	else if #2 d1 < #2 d2 then true
	else if #2 d1 > #2 d2 then false
	else if #3 d1 < #3 d2 then true
	else false

fun number_in_month(days: (int*int*int) list, month: int) =
	if null days then 0
	else number_in_month(tl days, month)
		+ (if #2 (hd days) = month then 1 else 0)

fun number_in_months(days: (int*int*int) list, months: int list) =
	if null months then 0
	else number_in_month(days, hd months)+number_in_months(days, tl months)

fun dates_in_month(days:(int*int*int) list, month: int) =
	if null days then []
	else (if #2 (hd days) = month then hd days :: dates_in_month(tl days, month) else dates_in_month(tl days, month))

fun dates_in_months(days: (int*int*int) list, months: int list) =
	if null months then []
	else dates_in_month(days, hd months) @ dates_in_months(days, tl months)

fun get_nth(strs: string list, n: int) =
	if n = 1 then hd strs
	else get_nth(tl strs, n-1)

fun date_to_string(date: int*int*int) =
	let
		val months = ["January ", "February ", "March ", "April ", "May ", "June ", "July ", "August ", "September ", "October ", "November ", "December "]
	in
		get_nth(months, #2 date)^Int.toString(#3 date)^", "^Int.toString(#1 date)
	end

fun number_before_reaching_sum(sum: int, nums: int list) =
	if hd nums >= sum then 0
	else 1 + number_before_reaching_sum(sum - hd nums, tl nums)

fun what_month(n: int) =
	number_before_reaching_sum(n, [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31])+1

fun month_range(day1: int, day2: int) =
	if day1 > day2 then []
	else what_month(day1) :: month_range(day1+1, day2)

fun oldest(days: (int*int*int) list) =
	if null days then NONE
	else 
		let
			val tl_ans = oldest(tl days)
		in
			if isSome tl_ans andalso is_older(valOf tl_ans, hd days)
			then tl_ans			
			else SOME (hd days)
		end
fun remove_duplicates(l1: int list, l2: int list)=
	let 
		fun in_list(n: int, l: int list)=
		if null l then false
		else if n = hd l then true
		else in_list(n, tl l)
	in 
		if null l2 then l1
		else if in_list(hd l2, l1) then remove_duplicates(l1, tl l2)
		else remove_duplicates(hd l2 :: l1, tl l2)
	end

fun number_in_months_challenge(days: (int*int*int) list, months: int list) =
	let 
		val new_months = remove_duplicates([], months)
	in
		number_in_months(days, new_months)
	end

fun dates_in_months_challenge(days: (int*int*int) list, months: int list) =
	let
		val new_months = remove_duplicates([], months)
	in
		dates_in_months(days, new_months)
	end
val xxx=123