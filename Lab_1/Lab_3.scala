def potega (x:Float, n:Int):Float =
{
   if (n== 0) 1
    else if( n%2 == 0) 
    {
      val m = potega(x, (n/2)) 
      m*m
    }
    else 
    {
      val m = potega(x, (n/2))
      m*m*x
    }
  }

potega (2, 17);

def potegaR (x:Double, n:Int):Double =
{ def pom (n:Int,acc:Double, akk:Double):Double =
    if (n==0) acc
    else if ( n%2 == 0) (pom(n/2, acc, akk*akk))
    else  (pom (n/2,acc*akk, akk*akk))
   pom(n,1,x) 
}
  
  potegaR (2, 17);

  
  def succTail(n: Int) = {
def succIter(n:Int, accum:Int): Int =
if (n==0) accum else succIter(n-1, accum+1)
succIter(n,1)
}
  
  
  
