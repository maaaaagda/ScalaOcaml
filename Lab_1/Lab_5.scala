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