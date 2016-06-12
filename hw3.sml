(* Dan Grossman, CSE341, HW3 Provided Code *)

exception NoAnswer

datatype pattern = WildcardP
		 | VariableP of string
		 | UnitP
		 | ConstantP of int
		 | ConstructorP of string * pattern
		 | TupleP of pattern list

datatype valu = Constant of int
	      | Unit
	      | Constructor of string * valu
	      | Tuple of valu list

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    WildcardP         => f1 ()
	  | VariableP x       => f2 x
	  | ConstructorP(_,p) => r p
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = AnythingT
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | DatatypeT of string

(**** you can put all your code here ****)

fun only_lowercase xs =
  List.filter (fn s => Char.isLower(String.sub(s,0))) xs

fun longest_string1 xs =
  List.foldl (fn (s,t) => if String.size s > String.size t then s else t) "" xs

fun longest_string2 xs =
  List.foldl (fn (s,t) => if String.size s >= String.size t then s else t) "" xs

fun longest_string_helper f xs =
  List.foldl (fn (s,t) => if f (String.size s,String.size t) then s else t) "" xs

val longest_string3 = longest_string_helper (fn (s,t) => s > t)
val longest_string4 = longest_string_helper (fn (s,t) => s >= t)
					   
fun longest_lowercase xs =
  let val aux = longest_string1 o only_lowercase
  in
      aux xs
  end

fun rev_string str =
  let val helper = String.implode o rev o String.explode
      val ustr = String.map Char.toUpper str
  in
      helper ustr
  end

fun first_answer f xs =
  case xs of
      [] => raise NoAnswer
    | x::xs' => case f x of
		    NONE => first_answer f xs'
		  | SOME t => t

fun all_answer f xs =
  let fun aux f xs ret =
	case xs of
	    [] => ret
          | x::xs' => case f x of
			  NONE => aux f xs' ret
			| SOME t => aux f xs' (SOME (valOf ret @ t))
  in
      aux f xs (SOME [])
  end
      
val count_wildcard = g (fn () => 1) (fn s => 0)								      

val count_wildcard_and_variable_lengths = g (fn () => 1) (fn s => String.size s)

fun count_a_var (s,p) =
  g (fn () => 0) (fn str => if str=s then 1 else 0) p

fun check_pat p =
  let fun get_strs p acc =
	case p of
	    VariableP x => x::acc
	  | ConstructorP (_,p) => get_strs p acc
	  | TupleP ps => foldl (fn (p,acc) => get_strs p acc) acc ps
	  | _ => acc
      fun is_unique xs =
	case xs of
	    [] => true
	  | x::xs' => if List.exists (fn s => if s=x then true else false) xs'
		      then false
		      else is_unique xs'
      val strs = get_strs p []
  in
      is_unique strs
  end

exception MisMatch
fun match (v,p) =
  (
  case (v,p) of
      (_,WildcardP) => SOME []
    | (_,VariableP s) => SOME [(s,v)]
    | (Unit,UnitP) => SOME []
    | (Constant a,ConstantP b) => if a=b then SOME [] else raise MisMatch
    | (Constructor (s1,v'),ConstructorP (s2,p')) => if s1=s2 then match (v',p') else raise MisMatch
    | (Tuple vs,TupleP ps) => let val pairs = ListPair.zipEq (vs,ps) in all_answer match pairs end
    | (_,_) => raise MisMatch
  )
  handle MisMatch => NONE
       | UnequalLengths => NONE
					       
fun first_match v pl =
  SOME (first_answer (fn p => match (v,p)) pl)
  handle NoAnswer => NONE
			 

				      
      
