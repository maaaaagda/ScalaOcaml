
zad 2
let rec fib p= 
	match p with
	| 0->0
	| 1->1
	| _ -> fib(p-1)+fib(p-2) ;;

fib(42);;

let rec fibpom (a, b, n) = 
	match n with
	| 0 -> a
	| _ ->fibpom (b, (a + b), (n - 1));;
	

let fibn n = fibpom (0, 1, n);;
fibn 42;;


zad 3
let rec root3 (a, e)=
	match (a>1.) with
	| true -> root3pom(a, a/.3., e)
	| false ->root3pom(a, a, e);;

 

let rec root3pom (a, x, e) = 
	match (absF((x**3.)-.a)<=e*.absF(a)) with
	| true -> x
	| false ->root3pom (a, x+.((a/.(x**2.))-.x)/.3., e);;

let absF (x)=
	match x>=0. with
	| true -> x
	| false -> -.x ;;

	root3(1000., 10.**(-1.));;


zad 4
let a = [-2; -1; 0; 1; 2];;
let [_; _; x; _; _] = a;;

let b = [(1,2);(0,1)];;
let [(_,_);(x,_)] = b;;

zad 5
let rec initSegment (xs, xm) = 
	match (xs, xm) with 
	| [],_ ->true
	| _,[]->false
	| hd1::tl1,hd2::tl2 when(hd1=hd2) ->initSegment(tl1, tl2)
	| _ -> false;;

	initSegment(3::2::7::[], []);;
zad 6

let rec replaceNth (ls, l, z) =
	match (ls, l) with
	| [], _ ->[]
	| hd::tl,0 ->z::tl
	| hd::tl, _->hd::replaceNthhh(tl, l-1, z);; 
