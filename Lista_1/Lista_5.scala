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
  

	
	
	