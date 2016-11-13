def sumProd(xs:List[Int]):(Int,Int) =
  xs match {
  case h::t => {val (s,p)=sumProd(t)
    (h+s,h*p)
  }
  case Nil => (0,1)
}

def sumProd1(xs:List[Int]) =
  (xs.foldLeft (0, 1)) { case ((a, b), x)=>(a+x, b*x) }
  
sumProd1(List(2,2,2))

def suum(xs:List[Int]) =
  xs.foldLeft(0)((x, y) => x+y)
  
  
val a = List(2,3,4,5);
val b = 10;
val c = List(1,2,3,4,5);
val m = c:::a;
val n = a++c;
  
  
  