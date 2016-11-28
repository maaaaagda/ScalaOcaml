//zadanie 1
let f1 x = x 1 1;;
let f1 = function x -> x 1 1;;
val f1 : (int -> int -> 'a) -> 'a = <fun>

//zadanie 3
let sumProd  = 
	List.fold_left(fun (a, b ) y ->(a+y, b*y)) (0, 1) ;;

sumProd [2;2;4];;

//zadanie 4
let rec quicksort = function
[] -> []
| [x] -> [x]
| xs -> let small = List.filter (fun y -> y < (List.hd xs) ) (xs)
and large = List.filter (fun y -> y >= (List.hd xs) )(List.tl xs)  
in quicksort small @ (List.hd xs::quicksort large);;

quicksort [5;3;3;4;6;7;3;8];;
(* zapêtla³a siê program bo zawsze w large pojawi siê piwot, wiec trzeba go do³¹czyc *)

let rec quicksort' = function
[] -> []
| x::xs -> let small = List.filter (fun y -> y < x ) xs
and large = List.filter (fun y -> y >= x ) xs
in quicksort' small @ (x :: quicksort' large);;

quicksort' [5;4;6;7;4;3;8;];;
(* brakowalo = w linice z large wiec ucinane byly liczby rowne piwotom *)
let f2 x y z = x ( y ^ z );;
let f2 = function x -> function y-> function z -> x(y^z);;
val f2 : (string -> 'a) -> string -> string -> 'a = <fun>

let f z y x = x( y z);;
let f z y x = x y z;;
let f x y z = z x x (y x);;
let f a b c = c a::b;;
let f x y z = z x (y x);;
let f x y z = x z (y x);;
let f x z y q= x z (y q);;

2
let dodaj= function a -> function b -> a+b;;

dodaj 5 10;;
let dodaj5 a =
	dodaj 5 a;;

let x = dodaj5 10;;
	5

let xs = [2;3;4];;
let m = xs
let f q y = (y q);;
let f q y z = z (y q);;
let f q y z x = x z (y q);;

let sigma f m = 
	let rec suma(i,s)=
		if i = m then s else suma(i+1, s+.f(i+1))
	in suma(0, f 0);;
	
sigma (fun k -> float(k*k)) 9;;	
let sqr k = float(k*k);;
sigma sqr 9;;

sigma( fun i -> sigma (fun j -> float (i+j)) 4) 3;;

let sigma2 g m n =
	sigma(fun m->sigma(fun n -> g(i, j)) n) m;;
sigma2 (fun (k, p) ->float(k+p)) 3 4;;

let f2 x = x 2 2;
let f3 x y z = x (y^z);;


let rec max_list smallest lst = match lst with 
    | []   -> smallest
    | h::t -> max_list (max smallest h) t;;

max_list 0 [2;3;7;0;10;3];;

let max_list1 smallest lst =
    List.fold_left (fun acc x -> max acc x) smallest lst;;
max_list1 0 [2;3;7;0;10;3];;

let min_list1 biggest lst =
    List.fold_left (fun acc x ->  acc+ x) min lst;;
max_list1 0 [2;3;7;0;10;3];;

let sum l=
  List.fold_left (fun acc x -> acc + x) 0 l;;
sum([2;3;7;0;10;3]);;



let rec sumProd l =
	match l with
	| h::t -> let (s,p)=sumProd t
	 in (h+s, h*p)
	| [] -> (0, 1);;

sumProd [2;2;2];;

let sumProdFold  = 
	List.fold_left(fun (a, b ) y ->(a+y, b*y)) (0, 1) ;;

sumProdFold [2;2;2];;

let sumProdA f l
(fun l ->List.fold_left(fun (a, b ) y ->(a+y, b*y)) (0, 1) l) [2;2;2];;

let curry f x y = f (x, y);;
let uncurry f (x, y) = f x y;;

let replicate list =
  let rec impl(list, index, nth, power, result) = 
    match list with
	h :: t ->
	  if nth <= index then
	    impl(list, index, nth + 1, power * h, (power * h) :: result)
	  else
	    impl(t, index + 1, 1, 1, result)
      | [] -> List.rev result
  in
  impl(list, 1, 1, 1, []);;

replicate [1; 2; 3; 4; 5];;

let filter p =
let rec find acc = function
[] -> List.rev acc
| x :: xs -> if p x then find (x :: acc) xs else find acc xs
in
find [];;

let primes to_n =
let rec sieve n =
if n <= to_n then n::sieve(n+1) else []
and find_primes l =
match l with
h::t -> h:: find_primes (List.filter (fun x -> x mod h <> 0) t)
| [] -> []
in find_primes(sieve 2);;

let even to_n =
let rec sieve n =
if n = to_n then []
else if(n mod 2 = 0) then n::(sieve (n+1))
else (sieve (n+1))
	in sieve 0;;

even 30;;

let rec insert poprzedza elem l =
match l with
[] -> [elem]
| h::t as l -> if poprzedza elem h then elem::l
else h::(insert poprzedza elem t);;

type kolor = Trefl | Karo | Kier | Pik;;


| Dama of kolor | Krol of kolor | As of kolor;;
type karta =
Blotka of kolor * int
| Walet of kolor
| Dama of kolor
| Krol of kolor
| As of kolor;;


let rec quicksort = function
[] -> []
| [x] -> [x]
| xs -> let small = List.filter (fun y -> y < (List.hd xs) ) (xs)
and large = List.filter (fun y -> y > (List.hd xs) )( xs)
and mid = List.filter (fun y -> y == (List.hd xs)) xs
in quicksort small @ mid @quicksort large;;

quicksort [5;3;3;4;6;7;3;8];;

let rec quicksortm = function
[] -> []
| [x] -> [x]
| xs -> let small = List.filter (fun y -> y < (List.hd xs) ) (xs)
and large = List.filter (fun y -> y >= (List.hd xs) )(List.tl xs)

in quicksortm small @(List.hd xs ::quicksortm large);;

quicksort [5;3;4;6;7;3;8;];;
let rec quicksort' = function
[] -> []
| x::xs -> let small = List.filter (fun y -> y < x ) xs
and large = List.filter (fun y -> y >= x ) xs
in quicksort' small @ (x :: quicksort' large);;

quicksort' [5;4;6;7;4;3;8;];;


# let curry f x y = f (x, y);;




let xs = [5;6;6;3;23;1;10];;

let rec insertionSort kryt list=
	let rec insert elem sorted =
		match elem, sorted with
		| _,[]->[elem]
		|_,h::t-> if (kryt elem h) then elem::h::t
							else h::(insert elem t)
	in match list with
	| []->[]
	| h::t->insert h (insertionSort kryt t);;	(*in insert nbnnb result*)
	
let x= insertionSort (<) xs;;	

let xss = [5;6;2;3;100];;
let rec insertionSort1 kryt list=
	let rec insert1 listt sorted =
		match listt, sorted with
		|[],_->sorted
		|h::t, [] -> (insert1 t [h])
		|h::t, h1::t1-> if (kryt h h1) then (insert1 t (h::sorted))
							else (h1::(insert1 listt t1))
	in insert1 list [];;

let x= insertionSort1 (>) xss;;	


