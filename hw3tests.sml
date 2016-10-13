val strs1 = ["Ricky","zhang","hello","Allen","jay","Chou"]

val oret = only_lowercase strs1

val lret1 = longest_string1 strs1
val lret2 = longest_string2 strs1

val llret = longest_lowercase strs1

val rret1 = rev_string "foo"
val rret2 = rev_string "heLLo"

val pat1 = TupleP [WildcardP, VariableP "foo",UnitP,ConstantP 4,ConstructorP ("bar",VariableP "abc")]

val pat2 = TupleP [WildcardP,TupleP [WildcardP,TupleP [WildcardP,UnitP],ConstantP 4],ConstructorP ("bar",WildcardP)]

val pat3 = TupleP [VariableP "foo",ConstructorP ("bar",VariableP "foo")]

val wret1 = count_wildcard pat1 (* 1 *)
val wret2 = count_wildcard pat2 (* 4 *)

val vret1 = count_wildcard_and_variable_lengths pat1 (* 7 *)
val vret2 = count_wildcard_and_variable_lengths pat2 (* 4 *)
val vret3 = count_wildcard_and_variable_lengths pat3 (* 6 *)

val cret1 = count_a_var ("foo",pat1) (* ret1 = 1 *)
val cret2 = count_a_var ("foo",pat2) (* ret2 = 0 *)
val cret3 = count_a_var ("foo",pat3) (* ret3 = 2 *)

val pret1 = check_pat pat1 (* true *)
val pret2 = check_pat pat3 (* false *)

val pat4 = WildcardP
val pat5 = TupleP [VariableP "ricky",VariableP "zhang"]
		  
val plret = [check_pat pat1,check_pat pat2,check_pat pat3,check_pat pat4,check_pat pat5] 
		  
val pl = [pat1,pat2,pat3,pat4,pat5]

fun check_pat_wrap1 p =
  if check_pat p
  then SOME p
  else NONE

fun check_pat_wrap2 p =
  if check_pat p
  then SOME [p]
  else NONE
	   
val fret = first_answer check_pat_wrap1 pl
val aret = all_answer check_pat_wrap2 pl

val p1 = WildcardP
val v1 = Constructor ("foo",Constant 18)
val v2 = Tuple [Unit,Constant 18]

val mret1 = match (v1,p1)
val mret2 = match (v2,p1)

val p2 = VariableP "foo"

val mret3 = match (v1,p2)
val mret4 = match (v2,p2)

val p3 = UnitP
val v3 = Unit

val mret5 = match (v3,p3)
val mret6 = match (v1,p3)

val p4 = ConstantP 17
val v4 = Constant 17

val mret7 = match (v4,p4)

val p5 = ConstructorP ("bar",TupleP [UnitP,VariableP "he",ConstantP 17])
val v5 = Constructor ("bar",Tuple [Unit,Constant 18,Constant 17])

val mret8 = match (v5,p5)

val p6 = TupleP [UnitP]
val v6 = Tuple [Unit,Constant 18]

val mret9 = match (v6,p6)

val p_list1 = [p1,p2,p3,p4]
val pret1 = first_match v6 p_list1

val p_list2 = [p5,p6,p4]
val pret2 = first_match v5 p_list2 
			
