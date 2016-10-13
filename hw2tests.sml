(* Dan Grossman, CSE341, HW2 Provided Tests *)

(* These are just two tests for problem 2; you will want more.

   Naturally these tests and your tests will use bindings defined 
   in your solution, in particular the officiate function, 
   so they will not type-check if officiate is not defined.
 *)
val strss1 = [["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]]
val at1 = all_substitutions1 (strss1,"Fred")
val strss2 = [["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]]
val at2 = all_substitutions1 (strss2,"Jeff")

val at3 = all_substitutions2 (strss1,"Fred")			     
val at4 = all_substitutions2 (strss2,"Jeff")

val strss = [["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]]
val name = {first="Fred", middle="W", last="Smith"}
val st = similar_name(strss,name)
		     
val cards = [(Jack,Clubs),(Num 4,Spades),(King,Hearts),(Num 8,Hearts)]
val t1 = remove_card(cards,(Num 4,Spades),IllegalMove)
		    (* val t1 = remove_card(cards,(Num 7,Spades),IllegalMove) *)
		    
val cards1 = cards
val cards2 = [(Jack,Clubs),(Ace,Spades),(King,Clubs),(Jack,Spades)]
val t2 = all_same_color cards1
val t3 = all_same_color cards2
			
val t4 = sum_cards cards1
val t5 = sum_cards cards2

val t6 = score (cards1,35)
val t7 = score (cards2,35)

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

(* val t8 = provided_test1 () *)
val t9 = provided_test2 ()


