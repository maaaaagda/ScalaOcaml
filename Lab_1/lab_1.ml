
let zad1a( x,  y) = 
  x-5<>y-5;;

let zad1b (x, y) = 
	(x+.5.0,y+.5.0);;


let zad1c(xs, x) =
	if( x = 0 ) then []
	else (List.hd xs) :: List.tl xs;;



zad 2 a
let rec lastTwo xm = 
	if( xm = [] || List.tl xm =[]) then failwith "blad"
	else if (List.tl(List.tl(xm))=[]) then (List.hd(xm)),(List.hd(List.tl xm))
  else lastTwo(List.tl xm);;

	
lastTwo(5::6::7::8::[]);;
lastTwo([]);;

zad 3		
let rec dwaRowne xm =
	if xm = [] then false
	else if ((List.tl xm) = []) then false
	else if (List.hd xm = List.hd(List.tl(xm))) then true
	else dwaRowne(List.tl xm);;  
	
	let x = 5::6::6::[];;
  dwaRowne x;;