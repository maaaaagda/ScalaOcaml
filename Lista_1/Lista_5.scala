///strumienie

val ss = Stream.cons(1,Stream.cons(2,Stream.cons(3,Stream.empty)))
s force

def primes = {
def sieve(s:Stream[Int]):Stream[Int] = {
val p #:: xs = s
p #:: sieve(xs filter (n => n%p != 0))
}
sieve(Stream.from(2))
}
primes.take(6).toList

//zadanie 1
def repeat[A](n:Int, s:Stream[A]):Stream[A] = 
{
  def aux[A](counter:Int, l:Stream[A]):Stream[A]=  l match
  {
    case Stream.Empty => Stream.Empty 
    case h#::tl => if (counter == n) aux (0, tl)
                  else h#::aux(counter+1, l)
  }
  aux (0, s)
}
  
  (repeat (5, ss)).toList
  
//zadanie 2
def fibonacci=
{
   def fibAux(current:Int, next:Int):Stream[Int]=current#::fibAux (next, current+next)
   fibAux (1, 1)
}
 fibonacci.take(10).toList

sealed trait lBT[+A]
case object LEmpty extends lBT[Nothing]
case class LNode[+A](elem:A, left:()=>lBT[A], right:()=>lBT[A]) extends lBT[A]
//zadanie 3 a
  
def  lTree (n:Int):lBT[Int] = LNode(n, (()=>lTree (2*n)), (()=>lTree (2*n+1)))

//zadanie 3 b  
def lTreeTol [A](lt:lBT[A]):Stream[A]  =
{
    def aux(llt:List[lBT[A]]):Stream[A] = 
      llt match
               {
                case Nil => Stream.Empty
                case LEmpty::t =>aux (t)
                case LNode(v, l, p)::t => v#::(aux (t:::List(l(), p()))) 
                }
  aux (List(lt))
}
  
lTreeTol(lTree(1)).take(10).toList
	
	