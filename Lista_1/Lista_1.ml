let palindrome xs =
if xs=[] then failwith "pusta lista"
else xs = List.rev xs;;
let xd = 1. :: 5.5 :: 1. :: [];;
let xm = [];;
palindrome xm;;

let rec len xs =
	if xs=[] then 0
	else 1 + len (List.tl xs);;
let xd = 'a' :: 'b' :: 'a' :: [];;
len xd;;
 + len List.tl xs


let rec sqrList0 xs = 
	if xs=[] then  []
	else List.hd xs*List.hd xs::sqrList(List.tl xs);;
	sqrList0([1;2;3;4]);;

((List.hd xs) * (List.hd xs)) sqrList (List.tl xs);;
(List.hd xs) * (List.hd xs)



let rec sqrList xs =
	if(List.length xs <1) then []
	else List.hd xs*List.hd xs::sqrList(List.tl xs);;

sqrList(9::[]);;

let v1 = 5:: 9::8::7::[];;



Zdefiniuj funkcjê replicate: 'a * int -> 'a list powtarzaj¹c¹ dany obiekt okreœlon¹ liczbê
 razy i zwracaj¹c¹ wynik w postaci listy, np. replicate ("la",3) zwraca ["la"; "la"; "la"].



let rec replicate k =
	if ((snd k) <= 0) then []
	else fst k::replicate (fst k, snd k -1);; 
	replicate("la", 4);;


let k3 = (2,'a');;


2. Zdefiniuj funkcjê count : 'a * 'a list -> int obliczaj¹c¹ ile razy dany obiekt wystêpuje
 w danej liœcie, np. count ('a', ['a'; 'l'; 'a']) zwraca 2. 

let rec count k = 
	if (List.length (snd k) = 0) then 0
	else if (List.hd (snd k) = fst k) then 1 + count (fst k, List.tl (snd k))
	else 0 + count (fst k, List.tl (snd k));;
count ('a', ['a'; 'l'; 'a']);;


flatten [[5;6];[1;2;3]] zwraca [5; 6; 1; 2; 3]
'a list list -> 'a list
list glowa @ lista ogon


let rec flatten lol =
	if lol=[] then []
	else (List.hd lol)@(flatten (List.tl lol));;

flatten [[5;6];[];[1;2;3]];;



