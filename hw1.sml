fun is_older(first:int * int * int,second:int * int * int) =
  let 
    val nf = [#3 first,#2 first,#1 first]
    val ns = [#3 second,#2 second,#1 second]
    fun is_older_helper(first:int list,second:int list) =
      if null first
      then false
      else if hd first < hd second
      then true
      else if hd first > hd second 
      then false
      else is_older_helper(tl first,tl second)
  in
    is_older_helper(nf,ns)
  end

fun number_in_month(dates:(int * int * int) list,month:int) =
  if null dates
  then 0
  else if #2 (hd dates) = month
  then 1 + number_in_month(tl dates,month)
  else number_in_month(tl dates,month)

fun number_in_months(dates:(int * int * int) list,months:int list) =
  if null months
  then 0
  else number_in_month(dates,hd months) + number_in_months(dates,tl months)

fun dates_in_month(dates:(int * int * int) list,month:int) =
  if null dates
  then []
  else if #2 (hd dates) = month
  then hd dates :: dates_in_month(tl dates,month)
  else dates_in_month(tl dates,month)

fun dates_in_months(dates:(int * int * int) list,months:int list) =
  if null months
  then []
  else
      let val x = dates_in_month(dates,hd months)
	  val y = dates_in_months(dates,tl months)
	  fun append(xs:(int * int * int) list,ys:(int * int * int) list) =
	    if null xs
	    then ys
	    else hd xs :: append(tl xs,ys)
      in
	  append(x,y)
      end

fun get_nth(strs:string list,n:int) =
  if n = 1
  then hd strs
  else get_nth(tl strs,n-1)

fun date_to_string(date:int * int * int) =
  let
      val months = ["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"]
  in
      get_nth(months,#2 date) ^ "-" ^ Int.toString(#1 date) ^ "-" ^ Int.toString(#3 date)
  end

fun number_before_reaching_sum(sum:int,nums:int list) =
  if hd nums >= sum
  then 0
  else 1+number_before_reaching_sum(sum - (hd nums),tl nums)

fun what_month(n:int) =
  let val days = [31,28,31,30,31,30,31,31,30,31,30,31]
  in 1 + number_before_reaching_sum(n,days)
  end

fun month_range(day1:int,day2:int) =
  if day1 > day2
  then []
  else
      let val m1 = what_month(day1)
	  val m2 = what_month(day2)
	  fun get_range(low:int,high:int) =
	    if low > high
	    then []
	    else low::get_range(low+1,high)
      in
	  get_range(m1,m2)
      end
	  
fun oldest(dates:(int * int * int) list) =
  if null dates
  then NONE
  else
      let val r = hd dates
	  val m = oldest(tl dates)
      in
	  if isSome m andalso is_older(valOf m,r)
	  then m
	  else SOME r
      end

fun cumulative_sum(nums:int list) =
  let fun cumulative_sum_helper(nums:int list,sum:int) =
	if null nums
	then []
	else hd nums + sum :: cumulative_sum_helper(tl nums,hd nums + sum)
  in
      cumulative_sum_helper(nums,0)
  end

fun number_in_months_chal(dates:(int * int * int) list,months:int list) =
  let
      val ret = []
      fun is_not_in(t:int,nums:int list) =
	if null nums
	then true
	else if hd nums = t
	then false
	else
	    is_not_in(t,tl nums)


      fun unique(months:int list,ret:int list) =
	if null months
	then ret
	else if is_not_in(hd months,ret) 
	then
	    unique(tl months,hd months :: ret)
	else unique(tl months,ret) 
  in
      number_in_months(dates,unique(months,[]))
  end

(*
fun is_not_in(t:int,nums:int list) =
	if null nums
	then true
	else if hd nums = t
	then false
	else
	    is_not_in(t,tl nums)

      
fun unique(months:int list,ret:int list) =
	if null months
	then ret
	else if is_not_in(hd months,ret) 
	then
	    unique(tl months,hd months :: ret)
	else unique(tl months,ret)       
*)
      
fun reasonable_date(date:int * int * int) =
  let fun unreasonable_month(month:int) =
	if month >=1 andalso month <=12
	then false
	else true

      fun unreasonable_day(date:int * int * int) =
	let
	    val r_days = [31,28,31,30,31,30,31,31,30,31,30,31]
	    val l_days = [31,29,31,30,31,30,31,31,30,31,30,31]
	    fun leap_year(year:int) =
	      if (year mod 400) = 0 orelse (year mod 4) = 0 andalso (year mod 100) <> 0
	      then true
	      else false

	    fun get_nd(n:int,nums:int list) =
	      if n = 1
	      then hd nums
	      else get_nd(n-1,tl nums)

	    fun unreasonable(day:int,month:int,days:int list) =
	      let val nd = get_nd(month,days)
	      in
		  if day>=1 andalso day<=nd
		  then false
		  else true
	      end
	in
	    if leap_year(#3 date)
	    then unreasonable(#1 date,#2 date,l_days)
	    else unreasonable(#1 date,#2 date,r_days)
	end
  in    
      if #3 date <= 0
      then false
      else if unreasonable_month(#2 date)
      then false
      else if unreasonable_day(date)
      then false
      else true
  end
      
