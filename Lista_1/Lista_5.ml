
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

// modul lazyy************************************************************************************************************************************
let x = lazy (true||false, 3*4);;
Lazy.force x;;


//modu³ lazyy w listach leniwych
type 'a llist = LNil | LCons of 'a * 'a llist Lazy.t;;
let lhd = function
LNil -> failwith "lhd"
| LCons (x, _) -> x;;
let ltl = function
LNil -> failwith "ltl"
| LCons (_, lazy t) -> t;;
let rec lfrom k = LCons (k, lazy (lfrom (k+1)));;
let rec ltake = function
(0, _) -> []
| (_, LNil) -> []
| (n, LCons(x,lazy xs)) -> x::ltake(n-1,xs);;
let rec lsquares = function
LNil -> LNil
| LCons(x,lazy xs) -> LCons(x*x, lazy(lsquares xs));;
ltake (6, lsquares(lfrom 3));;
let rec lmap f = function
LNil -> LNil
| LCons(x,lazy xs) -> LCons(f x, lazy(lmap f xs));;
let sqr_llist = lmap (function x -> x*x);;

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


//zadanie 2
let fibonacci=
	let rec countFib current next=
		LCons(current, function()->countFib next(current + next))
	in countFib 1 1;;	

ltake(10, fibonacci);;

//zad 3 leniwe drzewa
type 'a lBT = LEmpty | LNode of 'a * (unit ->'a lBT) * (unit -> 'a lBT);;

//zadanie 3 a
let rec lTree n= LNode(n, (function() ->lTree (2*n)), (function() ->lTree(2*n+1)));;

create 5;

///zadanie 3 b
let ltreeTol lt = 
		let rec aux = function
		|  [] -> LNil
		| LEmpty::t ->aux t
		| LNode(v, l, p)::t ->LCons(v, function() ->aux  (t@(l()::p()::[])) )
	in aux[lt];;

ltake (299 ,ltreeTol (lTree 1));;

	let lazyTree = LNode(1, (function() ->LNode(2,
																								(function()->LNode(4, (function()->LEmpty), (function()->LEmpty))),
																								(function()->LNode(5,  (function()->LEmpty),  (function()->LEmpty)))
													)									), 
																								
													(function() ->LNode(3,
																								(function()->LNode(6, (function()->LEmpty), (function()->LEmpty))),
																								(function()->LNode(7, (function()->LEmpty), (function()->LEmpty)))
													)                   )
											 );;