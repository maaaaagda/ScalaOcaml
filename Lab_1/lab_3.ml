let (=>) a b =
	match (a, b) with
	| (true, false)-> true
	| (false, true)-> false
	| _ -> false;;

false => false;;

let sumMod l n =
  List.fold_left (fun acc x -> acc + x mod n ) 0 l;;

sumMod [2;3;7;0;10;-3] 5;;