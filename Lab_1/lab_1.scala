
zad 4
def dwaRowne[A] (xs:List[A]):Boolean =
  if (xs == Nil) false
  else if (xs.tail == Nil) false
  else if (xs.head == xs.tail.head) true
  else dwaRowne(xs.tail)

  dwaRowne(List())
  
zad 2 b
  
def twoLast[A] (x:List[A]):(A, A) =
 if(x == Nil || x.tail == Nil) throw new Exception("za krotka lista")
 else if(x.tail.tail == Nil) (x.head, (x.tail).head)
 else twoLast(x.tail)

twoLast(List(1,3,3,2, 7,8)) 