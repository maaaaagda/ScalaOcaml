(function x -> x+x) 6;; 

let nth = (-2, -1, 0, 1, 2);;
let (-2, -1, x, 1, 2) = nth;;
let (a, b, c, d, f) = nth;;

let sth = (true, "hello", 0);;
let (b, s, i)=sth;;
let sth = (true,"hello",0);;
let (_,_,x) = sth;;


let a = [-2; -1; 0; 1; 2];;
let [_; _; x; _; _] = a;;

let b = [(1,2);(0,1)];;
let [(_,_);(x,_)] = b;;

let ntt =  ((1,2),(0,1));;
let ((_,_),(x,_)) = ntt;;

let [(a,b);(c,d)] = nt;;
let[(1,y);(x,1)] = nt;;

let k = (true, 5.0);;
let(f, g) = k;;
let(true, x) = k;;

let rec fib p= 
	match p with
	| 0->0
	| 1->1
	| _ -> fib(p-1)+fib(p-2) ;;

fib(42);;

let rec fibpom (a, b, n) = 
  if n = 0 then a else fibpom (b, (a + b), (n - 1));;
let fibn n = fibpom (0, 1, n);;
fibn 4;;
fib1(42);;





	
	replaceNth(5::8::9::1::3::4::5::[],100, 2);;
	
	let rec initSegment (xs, xm)=
		if(xs = []) then true
		else if(xm=[]) then false
		else if (List.hd xs <> List.hd xm) then false
		else  initSegment(List.tl xs, List.tl xm);;

	initSegment(3::[], 3::[]);;
let rec initSeg (xs, xm) = 
	match (xs, xm) with 
	| [],_ ->true
	| _,[]->false
	| hd1::tl1,hd2::tl2 -> match (hd1=hd2) with
													| true -> initSeg(tl1, tl2)
													| false ->false;;

	initSeg(3::2::7::[], []);;



	
	replaceNth(5::8::9::1::3::4::5::[],100, 2);;
		
	let rec unzip ps =
		match ps with
		| [] ->([],[])
		| (h1,h2)::t -> let(l1, l2)=unzip t in (h1::l1,h2::l2);;	
		
		




let rec replaceNthh(ls, l, z) =
	match l with
	| 0 -> z::(List.tl ls)
	| _ -> (List.hd ls)::replaceNthh(List.tl ls, l-1,z);; 
		
	replaceNthh([1;2;3;4;5;6;8;9;8;1],3,0);;	



let rec replaceNth (ls, l, z) =
	match (ls, l) with
	| [], _ ->[]
	| hd::tl,0 ->z::tl
	| hd::tl, _->hd::replaceNthhh(tl, l-1, z);; 

	replaceNth([1;2;3;4;5;6;8;9;8;1],3,0);;	

let rec root3 (a, e)=
	match (a>1.) with
	| true -> cus(a, a/.3., e)
	| false ->cus(a, a, e);;

 

let rec cus (a, x, e) = 
  if (abs_float((x**3.)-.a)<=e*.abs_float(a)) then x 
	else cus (a, x+.((a/.(x**2.))-.x)/.3., e);;


root3(1000., 10.**(-15.));;