val a = (4,8,2009)
val b = (5,8,2009)
val r1 = is_older(a,b) (* should be true *)
val r2 = is_older(b,a)

val d = [(1,4,2008),(6,4,2009),(18,5,2003),(2,4,2000)]
val n = number_in_month(d,4)

val nr = number_in_months(d,[4,5])

val dates = dates_in_month(d,4)

val dr = dates_in_months(d,[4,5])

val strs = ["ricky","zhang","kevin","garnett","allen","iverson"]
val str = get_nth(strs,4)

val nums = [1,2,3,4,5,6,7]
val nbr = number_before_reaching_sum(10,nums)

val dates = [(12,3,2007),(24,2,2008),(23,8,2009),(10,3,2007)]
val or = oldest(dates)

val nums = [12,13,14]
val cr = cumulative_sum(nums)

val nir = number_in_months_chal(dates,[3,3,4])

val rdr1 = reasonable_date ((29,2,2000))
val rdr2 = reasonable_date ((29,2,1000))
val rdr3 = reasonable_date ((12,13,1997))
val rdr4 = reasonable_date((32,3,1997))
val rdr5 = reasonable_date ((31,4,1997))
			   

