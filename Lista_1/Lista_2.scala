
zad 2
def fib (p:Int):Int =
  p match
  {case  0 =>0
   case  1 =>1
   case  _ =>fib(p-1)+fib(p-2)
     
  }
fib(42)
def fibpom (a:Int, b:Int, n:Int):Int=
  n match
  {
  case 0 =>a
  case _ => fibpom(b, a+b, n-1)
  }
def fibn (n:Int) = fibpom(0, 1, n);
fibn (42);
zad 3

def root3(a:Double, e:Double):Double = 
  a>1 match
    {
    case (true) => root3Pom(a, a/3, e)
    case (false) =>root3Pom(a, a, e)
    }
 
 	
	def root3Pom(a:Double, x:Double, e:Double):Double=
	  (absF(Math.pow(x,3)-a)<=e*absF(a)) match
	  {
	  case (true) =>x
	  case (false) => root3Pom (a, (x+(a/(x*x)-x)/3), e)
	  }
	
	def absF (x:Double):Double=
	 x>=0 match
	{
	   case true => x
	   case false => -x
	} 
	
root3(1000, Math.pow(10, -15))


zad 4
val a =List (-2, -1, 0, 1, 2)
val List (_, _, x, _, _) = a

val b = List((1,2),(0,1))
val List((_,_),(x,_)) = b

zad 5

def initSegment[A,B](xs:List[A], xm:List[B]):Boolean =
  (xs, xm) match
  {
  case (Nil, _) =>true
  case (_, Nil) => false
  case (h1::t1,h2::t2) if h1==h2 =>initSegment(t1, t2)
  case _ => false
  }
	initSegment(List(3,4,5,6,7), List(3,4,5,6,7,8,9))
	
zad 6

def replaceNth[A](ls:List[A], l:Int, z:A):List[A]=
  (ls,l) match
  {
  case (Nil, _) =>Nil
  case (hd::tl,0) =>z::tl
  case (hd::tl, _) => hd::replaceNth(tl, l-1, z)
  }
	
		replaceNth(5::8::9::1::3::4::5::Nil,2, 100);;