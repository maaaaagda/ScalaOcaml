//aliasy***********************************************************************************************************
type ParaIX[Param] = (Int,Param)

type ParaIF = ParaIX[Float]

//val x = (3, 3.14f) lub wymuszenie typu

val x:ParaIF = (3, 3.14f)

//sumy roz��czne***********************************************************************************************************
sealed trait Kolor
case object Trefl extends Kolor
case object Karo extends Kolor
case object Kier extends Kolor
case object Pik extends Kolor

sealed trait Karta
case class Blotka(kolor:Kolor,warto��:Int) extends Karta
case class Walet(kolor:Kolor) extends Karta
case class Dama(kolor:Kolor) extends Karta
case class Kr�l(kolor:Kolor) extends Karta
case class As(kolor:Kolor) extends Karta

val k2=Blotka(Karo,2);

val przedzia�:Int=>Int=>List[Int] = a=>b=>if (a >b) Nil else b::(przedzia�(a)(b-1))

//przedzia� (1)( 10);***********************************************************************************************************
def map[A,B](f:A=>B)(xs:List[A]):List[B] = xs match {
case Nil => Nil
case x::xs => f(x)::map(f)(xs)
}

def wszystkieKarty(kol:Kolor):List[Karta] = {
val figury = List(As(kol), Kr�l(kol), Dama(kol), Walet(kol))
val blotki = map ((n:Int)=>Blotka(kol,n)) (przedzia�(2)(10))
figury:::blotki
}
val kiery = wszystkieKarty(Kier)

//lista heterogeniczna***********************************************************************************************************

sealed trait AB[+T1,+T2]
case class A[+T1,+T2](e:T1) extends AB[T1,T2]
case class B[+T1,+T2](e:T2) extends AB[T1,T2]

val lStr = List("Ala ", "ma ", "kota")

val lInts = List(1,2,3)

val lsint:List[AB[String,Int]] =
(map((x:String)=>A(x))(lStr)):::(map((x:Int)=>B(x))(lInts))

def concatAndAdd(xs:List[AB[String,Int]]):(String,Int) =
xs match {
case Nil => ("", 0)
case x::xs => {(x, concatAndAdd(xs)) match {
case (A(str), (s,n)) => (str+s, n)
case (B(num), (s,n)) => (s, num+n)
}}
}
concatAndAdd(lsint)

//drzewa binarne***********************************************************************************************************

sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem:A, left:BT[A], right:BT[A]) extends BT[A]

val t = Node(1,Node(2,Empty,Node(3,Empty,Empty)),Empty)
val tt = Node(1,Node(2,Node(4,Empty,Empty),Empty),Node(3,Node(5,Empty,Node(6,Empty,Empty)),Empty))
//t elem // akcesor
//t left // akcesor
//t right // akcesor

def nodes[A](bt:BT[A]):Int = bt match {
case Empty => 0
case Node(_, tl, tr) => 1 + nodes(tl) + nodes(tr)
}

nodes(t)

def sciezkaWewnetrzna[A](bt:BT[A]):Int =
{
  def zlicz(bt:BT[A], x:Int):Int =
  {
    bt match
    {
      case Empty =>0
      case Node(_, l, p) => x+ zlicz(l, x+1) +zlicz(p, x+1)
    }
  }
   zlicz(bt,0)
}
sciezkaWewnetrzna(tt)

def sciezkaZewnetrzna[A](bt:BT[A]):Int =
{
  def zlicz(bt:BT[A], x:Int):Int =
  {
    bt match
    {
      case Empty =>x
      case Node(_, l, p) => zlicz(l, x+1) +zlicz(p, x+1)
    }
  }
   zlicz(bt,0)
}
sciezkaZewnetrzna(tt)
//wyj�tki i ich obsluga***********************************************************************************************************
class NotFound extends Exception

def assoc[A,B](key:A)(ps:List[(A,B)]):B = ps match {
case Nil => throw new NotFound
case (k,item)::ps => if (k==key) item else assoc(key)(ps)
}

val ps= List((1, "Alu"), (2, "Olu"))
"Witaj, " + assoc(3)(ps)

def szukaj[A] (klucz:A) (slownik:List[(A,String)]):String =
try {
  assoc(klucz)(slownik)
  } 
catch {
  case ex: NotFound => "niepowodzenie"
    }

"Witaj, " + szukaj(3)(ps)


// zad 3

sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem:A, left:BT[A], right:BT[A]) extends BT[A]

val tt = Node(1,Node(2,Node(4,Empty,Empty),Empty),Node(3,Node(5,Empty,Node(6,Empty,Empty)),Empty))

def breadthSearch[A](bt:BT[A]):List[A]=
{
  def pom[A](t:List[BT[A]]):List[A] =
  {
	  t match
	  {
    case  Nil => Nil
    case Empty::t => pom (t)
    case Node (v, l, p)::t => v::(pom (t:::List(l,p))) 
	  }
	}
   pom (List(bt))
}
breadthSearch (tt)

//zad 4
sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem:A, left:BT[A], right:BT[A]) extends BT[A]

val tt = Node(1,Node(2,Node(4,Empty,Empty),Empty),Node(3,Node(5,Empty,Node(6,Empty,Empty)),Empty))

def sciezkaWewnetrzna[A](bt:BT[A]):Int =
{
  def zlicz(bt:BT[A], x:Int):Int =
  {
    bt match
    {
      case Empty =>0
      case Node(_, l, p) => x+ zlicz(l, x+1) +zlicz(p, x+1)
    }
  }
   zlicz(bt,0)
}
sciezkaWewnetrzna(tt)

def sciezkaZewnetrzna[A](bt:BT[A]):Int =
{
  def zlicz(bt:BT[A], x:Int):Int =
  {
    bt match
    {
      case Empty =>x
      case Node(_, l, p) => zlicz(l, x+1) +zlicz(p, x+1)
    }
  }
   zlicz(bt,0)
}
sciezkaZewnetrzna(tt)

//zad 5
sealed trait Graphs[A]
case class Graph[A](succ: A=>List[A]) extends Graphs[A]

val g = Graph((i: Int) => i match {
case 0 => List(3)
case 1 => List(0,2,4)
case 2 => List(1)
case 3 => Nil
case 4 => List(0,2)
case n => throw new Exception("Graph g: node "+ n+ " doesn't exist")})

def breadthSearch[A] (g: Graph[A]) (startNode: A): List[A] = 
{
  def search(visited: List[A])(toVisit: List[A]): List[A] = toVisit match 
  {
    case Nil => Nil
    case h::t => if (visited contains h) search(visited)(t)
                 else h::search(h::visited)((g succ h)++t)
  }
search (Nil) (List(startNode))
}
breadthSearch (g) (4)