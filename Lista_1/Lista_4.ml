//typy*****************************************************************************************************

let curry f x y = f (x, y);;
let uncurry f (x, y) = f x y;;

let curry3 f x y z = x y z;;
let uncurry3 f (x, y, z) = f x y z;;
let f x y = y (x y);

let f2 x y z = x y z ;;

let f2 x y = function z -> x::y;;

let f2 x y = function z -> (x::y);;
let f2 x y = (function z -> x)::y;;
let f2 x y = function k ->(function z -> x::y);; (y)

let f2 x y z = z x::y;; === let f2 x y z = (z x)::y;; tak ida nawiaski

let f2 x y z = z (x::y);;
let f3 x  = function c ->x;;
let rec f4 x =f4 x;
let  f5 x =f5 x;

zad 2
let rec g x = g x;
let rec g1 x = x;
function x->x;;




type 'param para_i_x = int * 'param;;
type para_i_f = float para_i_x;;
let (x:para_i_f) = (3,3.14);;
let (x) = (3,3.14);;

//sumy roz��czne *****************************************************************************************************
type kolor = Trefl | Karo | Kier | Pik;;
type karta = Blotka of kolor*int | Walet of kolor
| Dama of kolor | Krol of kolor | As of kolor;;
let x2 = Krol Trefl;;
let (x3:karta) = Blotka (Karo, 6) ;;

let rec przedzial a b = if a>b then []
else b::(przedzial a (b-1));;

let wszystkieKarty kol =
let figury = [As kol; Krol kol; Dama kol; Walet kol]
and blotki = List.map (function n -> Blotka(kol,n))
(przedzial 2 10)
in figury @ blotki;;

let kiery = wszystkieKarty Kier;;

//lista heterogeniczna*****************************************************************************************************
type ('a,'b) ab = A of 'a | B of 'b;;
let ls = ["Ala "; "ma "; "kota"] and lint = [1;2;3];;
let lsint = (List.map (function x -> A x) ls) @ (List.map (function x -> B x) lint);;


let rec concat_and_add l =
match l with
[] -> ("",0)
| h::t -> match (h, concat_and_add t) with
(A str, (s,n)) -> (str^s, n)
| (B num, (s,n)) -> (s, num+n);;

concat_and_add lsint;;

//drzewa binarne*****************************************************************************************************
type 'a bt = Empty | Node of 'a * 'a bt * 'a bt;;

let t = Node(1,Node(2,Empty,Node(3,Empty,Empty)),Empty);;
let tt = Node(1,Node(2,Node(4,Empty,Empty),Empty),Node(3,Node(5,Empty,Node(6,Empty,Empty)),Empty));;
let ttt = Node(1,Node(2,Empty,Empty),Node(3,Empty,Empty));;
let rec nodes = function
Empty -> 0
| Node(_,t1,t2) -> 1 + nodes t1 + nodes t2;;

nodes t;;

let rec inorder = function
Node(v,l,r) ->(inorder l) @ v::(inorder r)
| Empty -> []
;;

let rec postorder = function
Node(v,l,r) ->(postorder l) @ (postorder r) @ [v]
| Empty -> [];;



let breadthSearch t =
	let rec pom = function
		| [] -> []
		| Empty::t -> pom t
		| Node (v, l, p)::t -> v::pom  (t@(l::p::[])) 
in pom [t];;

breadthSearch tt;;



	
let sciezkaWewnetrznaa bt =
	let rec zlicz(t, x) =
		match t with
		| Empty ->0
		| Node(_,l,p)-> x+zlicz(l,x+1)+zlicz(p,x+1)
in zlicz(bt,0);;	
	
	sciezkaWewnetrznaa tt;;	
let sciezkaZewnetrznaa bt =
	let rec zlicz(t, x) =
		match t with
		| Empty ->x
		| Node(_,l,p)-> zlicz(l,x+1)+zlicz(p,x+1)
in zlicz(bt,0);;	
	
	sciezkaZewnetrznaa tt;;	
//wyj�tki*****************************************************************************************************
exception Wow of string;;
let error r = raise(Wow r);;
let rec nodes = function
Empty -> raise (Wow "again...")
| Node(_,t1,t2) -> 1 + nodes t1 + nodes t2;;

nodes Empty;;
let hdt = function
[] -> error "there is no hd in here!"
| a::l -> a

let hd = function
| a::l -> a;;

hdt [];
hd[];
//trajkacze*****************************************************************************************************
let isHd x = 
	try (hd x) with Match_failure s-> -1;;

isHd [] ;


//typ opcjonalny*****************************************************************************************************

type 'a option = None | Some of 'a;;

let nodesOP l = 
	let rec nodess = function
	Empty -> 0
		|Node(_,t1,t2) ->1 +(nodess t1) +  (nodess t2)
	in match (nodess l) with
	| 0-> None
	| x -> Some x;;

let withOP l = 
	match (nodes l) with
	|0 -> None
	|y -> Some y;;
		
	withOP 	t;;
 	nodesOP t;;  //:D


//grafy*****************************************************************************************************
type 'a graph = Graph of ('a -> 'a list);;
let show (Graph p) nr = 
	p nr;;
show g 4;;

Graph g 4 ;;
let m = g 4;; ///fucking why? == pattern matching! kompilator musi gdzies to dopasowac bo inaczej nie wie o co kaman

let breadthSearch (Graph succ) startNode =
	let rec search visited = function
		[] -> []
		| h::t -> if List.mem h visited then search visited t
							else h::search (h::visited) (t @( succ h ))
in search [] [startNode];;

let breadthSearcha (Graph succ) startNode =
let rec search visited = function
	[] -> []
	| h::t -> ((succ h))
in search [][startNode];;

let depthSearch (Graph succ) startNode = 
	let rec search visited = function
	[] -> []
	| h::t -> if List.mem h visited then search visited t
						else h::search (h::visited) (( succ h )@t)
	in search [] [startNode];;
let g = Graph
(function
0 -> [3]
| 1 -> [0;2;4]
| 2 -> [1]
| 3 -> []
| 4 -> [0;2]
| n -> failwith ("Graph g: node "^string_of_int n^" doesn't exist")
);;
breadthSearcha g 4;;
breadthSearch g 4;;
depthSearch g 4;;

//zad 1
let f1 x y z = x y z ;;									f1 : ('a -> 'b -> 'c) -> 'a -> 'b -> 'c = <fun>

let f2 x y = function z -> (x)::y;;				f2 : 'a -> 'a list -> 'b -> 'a list = <fun>

//zad 2
let rec f x = f x;

let f x =	failwith "error";;

//zad 3
type 'a bt = Empty | Node of 'a * 'a bt * 'a bt;;

let tt = Node(1,Node(2,Node(4,Empty,Empty),Empty),Node(3,Node(5,Empty,Node(6,Empty,Empty)),Empty));;
let breadthSearch t =
	let rec pom = function
		| [] -> []
		| Empty::t -> pom t
		| Node (v, l, p)::t -> v::pom  (t@(l::p::[])) 
in pom [t];;

//zad 4

type 'a bt = Empty | Node of 'a * 'a bt * 'a bt;;
let tt = Node(1,Node(2,Node(4,Empty,Empty),Empty),Node(3,Node(5,Empty,Node(6,Empty,Empty)),Empty));;

let sciezkaWewnetrznaa bt =
	if bt==Empty then failwith "Empty!"
	else
	let rec zlicz(t, x) =
		match t with
		| Empty ->0
		| Node(_,l,p)-> x+zlicz(l,x+1)+zlicz(p,x+1)
in zlicz(bt,0);;	
	
sciezkaWewnetrznaa tt;;	

let sciezkaZewnetrznaa bt =
	let rec zlicz(t, x) =
		match t with
		| Empty ->x
		| Node(_,l,p)-> zlicz(l,x+1)+zlicz(p,x+1)
in zlicz(bt,0);;	

//zad 5
type 'a graph = Graph of ('a -> 'a list);;

let depthSearch (Graph succ) startNode = 
	let rec search visited = function
	|[] -> []
	| h::t -> if List.mem h visited then search visited t
						else h::search (h::visited) (( succ h )@t)
	in search [] [startNode];;

let g = Graph
(function
0 -> [3]
| 1 -> [0;2;4]
| 2 -> [1]
| 3 -> []
| 4 -> [0;2]
| n -> failwith ("Graph g: node "^string_of_int n^" doesn't exist")
);;
depthSearch g 1;;





