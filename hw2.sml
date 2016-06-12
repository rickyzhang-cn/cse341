(* Dan Grossman, CSE341, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun remove_option (str,strs) =
  let
      fun aux (str,strs,acc) =
	case strs of
	    [] => NONE
	  | s::strs' => if same_string(s,str)
			then SOME (acc @ strs')
			else aux(str,strs',s::acc)
  in
      aux (str,strs,[])
  end

fun all_substitutions1 (strss,str) =
  case strss of
      [] => []
    | strs::strss' => let val r = remove_option (str,strs)
		      in
			  case r of
			      NONE => all_substitutions1 (strss',str)
			    | SOME s => s @ all_substitutions1 (strss',str)
		      end

fun all_substitutions2 (strss,str) =
  let
      fun aux (strss,str,ret) =
	case strss of
	    [] => ret
	  | strs::strss' => let val r = remove_option(str,strs)
			    in
				case r of
				    NONE => aux(strss',str,ret)
				  | SOME s => aux(strss',str,ret@s)
			    end
  in
      aux (strss,str,[])
  end
      
fun similar_name (strss,name) =
  let
      fun aux (strs,name,ret) =
	case strs of
	    [] => ret
	  | str::strs' => case name of
			      {first=x,middle=y,last=z} => aux(strs',name,ret @ [{first=str,middle=y,last=z}])
								    
  in
      case name of
	  {first=x,middle=y,last=z} => let val r = all_substitutions2 (strss,x)
				       in
					   aux (r,name,[name])
				       end			  
  end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = rank * suit

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(* put your solutions for problem 2 here *)
fun card_color x =
  case x of
      (_, Clubs) => Black
    | (_, Diamonds) => Red
    | (_, Hearts) => Red
    | (_, Spades) => Black

fun card_value x =
  case x of
      (Ace, _) => 11
    | (Num i, _) => i
    | _ => 10

fun remove_card (cs,c,e) =
  let
      fun aux (cs,c,e,ret) =
	case cs of
	    [] => raise e
	  | x::cs' => if x = c
		      then ret @ cs'
		      else aux(cs',c,e,x::ret)
  in
      aux(cs,c,e,[])
  end

fun all_same_color cs =
  case cs of
      [] => true 
    | c1::cs1 => case cs1 of
		     [] => true
		   | c2::cs2 => if card_color(c1) = card_color(c2)
				then all_same_color cs1
				else false

fun sum_cards cs =
  let
      fun aux (cs,acc) =
	case cs of
	    [] => acc
	  | c::cs' => aux(cs',acc+card_value(c))
  in
      aux(cs,0)
  end

fun score (cs,goal) =
  let val is_same_color = all_same_color cs
      val sum = sum_cards cs
  in
      if sum > goal
      then
	  if is_same_color
	  then 5 * (sum - goal) div 2
	  else 5 * (sum - goal)
      else
	  if is_same_color
	  then (goal - sum) div 2
	  else (goal - sum)
  end
      
fun officiate (cs,ms,goal) =
  let
      fun aux (cs,ms,goal,hs) =
	case ms of
	    [] => score (hs,goal)
	  | m::ms' => case m of 
			  Discard c => let val hs' = remove_card (hs,c,IllegalMove) in aux (cs,ms',goal,hs') end
			| Draw => case cs of
				      [] => score (hs,goal)
				    | c::cs' => let
					val sum = sum_cards (c::hs)
				    in
					if sum > goal then score(c::hs,goal)
					else aux(cs',ms',goal,c::hs)
			
				    end							       
  in
      aux (cs,ms,goal,[])
  end

(*
fun provided_test1 () = (* correct behavior: raise IllegalMove *)
  let val cards = [(Jack,Clubs),(Num(8),Spades)]
      val moves = [Draw,Discard(Jack,Hearts)]
  in
      officiate(cards,moves,42)
  end

fun provided_test2 () = (* correct behavior: return 5 *)
  let val cards = [(Ace,Clubs),(Ace,Spades),(Ace,Clubs),(Ace,Spades)]
      val moves = [Draw,Draw,Draw,Draw,Draw]
  in
      officiate(cards,moves,42)
  end
*)

fun sum_cards_chal cs =
  let
      fun aux (cs,ret) =
	case cs of
	    [] => ret
	  | c::cs' => case c of
			  (Ace,_) => aux(cs',((#1 ret) + 1,(#2 ret) + 1))
			| _ => aux(cs',((#1 ret) + card_value(c),#2 ret))
  in
      aux(cs,(0,0))
  end
			
fun score_chal (cs,goal) =
  let fun calc_min (score,nums) =
	if nums = 0
	then score
	else
	    if (score - 10) < 0
	    then score
	    else calc_min(score - 10,nums - 1)
			 
      val is_same_color = all_same_color cs
      val ret = sum_cards_chal cs
  in
      if (#1 ret) > goal
      then
	  if is_same_color
	  then 5 * (#1 ret - goal) div 2
	  else 5 * (#1 ret - goal)
      else
	  let val min_score = calc_min(goal - (#1 ret),#2 ret)
	  in
	      if is_same_color
	      then min_score div 2
	      else min_score
	  end
  end 

fun officiate_chal (cs,ms,goal) =
  let
      fun aux (cs,ms,goal,hs) =
	case ms of
	    [] => score_chal (hs,goal)
	  | m::ms' => case m of 
			  Discard c => let val hs' = remove_card (hs,c,IllegalMove) in aux (cs,ms',goal,hs') end
			| Draw => case cs of
				      [] => score_chal (hs,goal)
				    | c::cs' => let
					val ret = sum_cards_chal (c::hs)
				    in
					if #1 ret > goal then score_chal (c::hs,goal)
					else aux(cs',ms',goal,c::hs)
			
				    end							       
  in
      aux (cs,ms,goal,[])
  end

fun careful_player (cs,goal) =
  let
      fun get_discard (sum,hs,goal) =
	let fun find_card (hs,value,t,oc) =
	      case hs of
		  [] => (valOf oc,remove_card(hs,valOf oc,IllegalMove))
	        | c::hs' => if card_value(c) = value
			    then (c,remove_card(hs,c,IllegalMove))
			    else
				if t < (value - card_value(c))
				then find_card(hs',value,t,oc)
				else find_card(hs',value,(value - card_value(c)),SOME c)
	in
	    find_card(hs,sum-goal,11,NONE)
	end
			    
      fun aux(cs,goal,ms,hs) =
	case cs of
	    [] => ms
	  | c::cs' => let val sum = sum_cards(c::hs)
		      in
			  if sum > goal
			  then let val (dc,hs') = get_discard(sum,hs,goal)
			       in
				   aux(cs,goal,(Discard dc)::ms,hs')
			       end
			  else if sum = goal
			  then Draw::ms
			  else aux(cs',goal,Draw::ms,c::hs)
		      end
  in
      aux(cs,goal,[],[])
  end
  
  
       
      
