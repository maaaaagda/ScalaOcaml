Zad 1
def flatten[A](xs:List[List[A]]):List[A] =
  if (xs == Nil) Nil
  else xs.head++flatten(xs.tail)
Zad 2
def count[A](x:A,m:List[A]):Int = 
  if (m == Nil) 0
  else if (x==m.head) 1+(count(x, m.tail))
  else (count(x, m.tail))

  count(1, List(1,2,1))
  
  def count[A](m:(A,List[A])):Int = 
if (m._2 == Nil) 0 
else if (m._1 == m._2.head) 1 + count ( m._1,m._2.tail) 
else 0 + count ( m._1,m._2.tail)

Zad 3
def replicate [A](k:(A,Int)):List[A]=
  if(k._2 <= 0) Nil
  else k._1::replicate(k._1, (k._2)-1)

Zad 4
def sqrList (xs:List[Int]):List[Int] =
  if(xs == Nil) Nil
  else (xs.head * xs.head)::sqrList(xs.tail)

Zad 5
def palindrome[A](xs:List[A]) = 
  if(xs == Nil)  throw new Exception("Pusta lista")
  else xs == xs.reverseZad 6

Zad 6
def listLength[A](xs:List[A]):Int = 
if (xs == Nil) 0
else 1 + listLength(xs.tail)







def listLength[A](xm: List[A]):Int = 
  if(xm == Nil) 0
  else 1+ listLength(xm.tail)


val x = 5::5.5::"Hello"::Nil;

listLength(x);




















