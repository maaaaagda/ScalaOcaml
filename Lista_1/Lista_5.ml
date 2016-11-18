
//listy leniwe**************************************************************************************************************************
type 'a llist = LNil | LCons of 'a * (unit -> 'a llist);;

let lhd = function
LNil -> failwith "lhd"
| LCons (x, _) -> x
;;

let ltl = function
LNil -> failwith "ltl"
| LCons (_, xf) -> xf()
;;
lhd ll1;;
let rec lfrom k = LCons (k, function () -> lfrom (k+1));;
let rec ltake = function
(0, _) -> []
| (_, LNil) -> []
| (n, LCons(x,xf)) -> x::ltake(n-1, xf())
;;

ltake (5,lfrom 30);;

let rec toLazyList = function
[] -> LNil
h::t -> LCons(h, function () -> toLazyList t);;

let rec (@$) ll1 ll2 =
match ll1 with
LNil -> ll2
| LCons(x, xf) -> LCons(x, function () -> (xf()) @$ ll2);;

let ll1 = LCons(2,function ()->LCons(1,function ()->LCons(4, function ()->LNil)));;
let ll2 = lfrom 3;;
let lpoj = LCons(4, function ()->LNil);;
ltake (10, ll1 @$ ll2);;

/////zadanie 1//////

let repeat m n=
	let rec aux counter l=
		match l with
		| LNil -> LNil
		| LCons(x,xf) ->  if counter = n then (aux (0) (xf()))
																		 else LCons(x, function () -> (aux (counter+1) l))
	in aux 0 m;;

ltake(6, ll1);;
ltake(60,repeat ll1 5);;


	let rec lmap f = function
	LNil -> LNil
| LCons(x,xf) -> LCons(f x, function () -> lmap f (xf()) )
;;

let sqr_llist = lmap (function x -> x*x);;

ltake (6, lll (lfrom 3));;


let rec lfilter pred = function
LNil -> LNil
| LCons(x,xf) -> if pred x
then LCons(x, function () -> lfilter pred (xf()) )
else lfilter pred (xf())
;;

let rec liter f x = LCons(x, function () -> liter f (f x));;
let primes =
	let rec sieve = function
	LCons(p,nf) -> LCons( p,	function () -> sieve(lfilter (function n -> n mod p <> 0)
(nf())
)
)
| LNil -> failwith "Impossible! Internal error."
in sieve (lfrom 2)
;;
ltake (6,primes);;

let rec ltakeWithTail = function
(0, xq) -> ([],xq)
| (_, LNil) -> ([],LNil)
| (n, LCons(x,xf)) ->let (l,tail)=ltakeWithTail(n-1, xf())
in (x::l,tail);;


/////zad 3 leniwe drzewa
Napisz funkcjê lTree , która dla zadanej liczby naturalnej n konstruuje nieskoñczone leniwe
drzewo binarne z korzeniem o wartoœci n i z dwoma poddrzewami lTree (2*n) oraz lTree( 2*n+1).
To drzewo jest przydatne do testowania funkcji z nastêpnego podpunktu.
type 'a lBT = LEmpty | LNode of 'a * (unit ->'a lBT) * (unit -> 'a lBT);;

let rec create =function
	| 0 -> LEmpty
	| n -> LNode(n, (function() ->create (2*n)), (function() ->create (2*n+1)));;

	create 5;
	let lazyTree = LNode(1, (function() ->LNode(2,
																								(LNode),()), 
													(function() ->create (2*n+1)))
//drzewa, breadthSearch
let breadthSearch t =
	let rec pom = function
		| [] -> []
		| Empty::t -> pom t
		| Node (v, l, p)::t -> v::pom  (t@(l::p::[])) 
in pom [t];;

breadthSearch tt;;

let ltreeTol lt = 
	let rec aux = function
		|  [] -> []
		| LEmpty::t ->aux t
		| LNode(v, l, p)::t ->v::aux  (t@(l()::p()::[])) 
	in aux[lt];;

ltreeTol (create 5);;