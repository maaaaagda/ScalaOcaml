
let xs = [100;50;20;10;5;2;1];;

let rec wydajReszte n l=
	match n, l with
	| 0, _ -> []
	| _, h::t -> if (n >= h) then h::(wydajReszte(n-h) (l))
							 else wydajReszte (n) (t);;	

let rec wydajResztef n l=
	match n, l with
	| 0, _ -> []
	| _, h::t -> (List.fold_left (fun acc x -> if(n>h) then x::acc else wydajResztef n t )[] l );;	


wydajResztef 129 xs;;















let filter p =
let rec find acc = function
[] -> List.rev acc
| x :: xs -> if p x then find (x :: acc) xs else find acc xs
in
find [];;

let sumMod l n =
  List.fold_left (fun acc x -> acc + x mod n ) 0 l;;

sumMod [2;3;7;0;10;-3] 5;;
let cos l n = List.filter (fun x -> x>n) l;;
cos [2;3;7;0;10;-3] 5;;




		
		let listyNiemalejace list =
    let rec aux current acc = function
      | [] -> []    
      | [x] -> (x :: current) :: acc
      | a :: (b :: _ as t) ->
         if a >= b then  (aux (a :: current) acc t)
         else List.rev (aux [] ((a :: current) :: acc) t  )
				in
     (aux [] [] (List.rev list));;


listyNiemalejace [];;

let breadthSearch (Graph succ) startNode =
let rec search visited = function
[] -> []
| h::t -> if List.mem h visited then search visited t
else h::search (h::visited) (t @ succ h)
in search [] [startNode];;		