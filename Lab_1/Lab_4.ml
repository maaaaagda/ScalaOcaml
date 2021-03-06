
//zadanie 1
type ('a,'b) ab = A of 'a | B of 'b;;

let k = [ A 5; B 5.5; A 6; B 7.5];;
let kk = [ A 5;  A 5; B 5.5; A 6; B 7.5];;
let rec naPrzemian l=
	match l with
	| [] -> true
	| [x] -> true
	| h::t ->match (h, List.hd t) with
					|(B _, A _) ->naPrzemian t; 
					|(A _, B _) ->naPrzemian t;
					|_ -> false 	
			
	;;			

naPrzemian k;
naPrzemian kk;

//zadanie 3

type osoba = {nazwisko:string; wiek:int; ocena:float};;

let osoby = [ {nazwisko = "nowak"; wiek =26; ocena = 4.5};
							{nazwisko = "nowak1"; wiek =20; ocena = 5.5};
							{nazwisko = "nowak2"; wiek =18; ocena = 3.5};
							{nazwisko = "nowak3"; wiek =36; ocena = 3.};]
							
let rec spelnia l= 
	match l with
	| [] -> failwith "pusta lista"		
	| h::t ->if(h.wiek>25 && h.ocena <4.) then h
					 else spelnia t;;	

spelnia osoby;

type 'a option = None | Some of 'a;;

let x=Some 4;;

let Some y=x;;
x+3;;

let tryCatch l =
	try let { nazwisko = nazwisko } = spelnia l in Some nazwisko with  
	| Failure "pusta lista" -> None
;;

let tryy l =
	try let x = (spelnia l).nazwisko in Some x
  with 	Failure "pusta lista" -> None;;	
			


tryCatch osoby;
tryy osoby;
