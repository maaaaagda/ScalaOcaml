let rec nTy (ls, l) =
	match (ls, l) with
	| [], _ -> failwith "error"
	| hd::tl,1 -> hd
	| hd::tl, _->nTy(tl, l-1);;

nTy([2;4],-8);;


nTy([2;3;5;6;7], 3);;
nTy([2;4],1);;


let rec podzielPoN(lista, dlugosc) = 
	match (lista, dlugosc) with
	| [], _ -> failwith "error"
	| hd::tl,1 ->let (l1) = podzielPoN (tl,dlugosc)  in ([hd], [tl]);;
	
	| hd::tl, _-> podzielPoN (lista,dlugosc-1);;

podzielPoN([2;3;5;6;7], 3);;















let rec podzielPon(l, num) =
	let rec pom (list1, list2, num) = 
		if (list2 = []) then (list1, list2) 
		else if (num > 0) then pom(list1 @ [List.hd list2], List.tl list2, num-1)
		else (list1,list2)

in pom([],l, num);;

		
podzielPon([1;3;5;4;0],3);;