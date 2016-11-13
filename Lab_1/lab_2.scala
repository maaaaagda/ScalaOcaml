
def nTy[A](ls:List[A], l:Int):A=
  (ls,l) match
  {
  case (Nil, _) => throw new Exception("Error") 
  case (hd::tl,1) =>hd
  case (hd::tl, _) => nTy(tl, l-1)
  }
	
	//	nTy(5::8::9::1::3::4::5::Nil,2)
		nTy(Nil,7)
		
		/*
let rec podzielPon(l, num) =
	let rec pom (list1, list2, num) = 
		if (list2 = []) then (list1, list2) 
		else if (num > 0) then pom(list1 @ [List.hd list2], List.tl list2, num-1)
		else (list1,list2)

in pom([],l, num);;
*/


def podzielPoN[A](xm:List[A], num:Int):(List[A], List[A]) =
{
  def pom [A](list1:List[A], list2:List[A], nr:Int):(List[A], List[A])=
    	if (list2 == Nil)  (list1, list2) 
    	else if (nr > 0)  pom(list1++List(list2.head), list2.tail, nr-1)
    	else (list1,list2)

  pom(Nil, xm, num)  	
}  
  
podzielPoN(5::8::9::1::3::4::5::Nil,2)
podzielPoN(5::8::9::1::3::4::5::Nil,-22)
podzielPoN(5::8::9::1::3::4::5::Nil,22)
  





