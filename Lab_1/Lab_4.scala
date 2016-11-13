
//zadanie 2
sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem:A, left:BT[A], right:BT[A]) extends BT[A]

val t = Node(1,Node(2,Empty,Node(3,Empty,Empty)),Empty)

def hgt[A](bt:BT[A]):Int = bt match {
case Empty => 0
case Node(_, tl, tr) => 1 + Math.max(hgt (tl), hgt (tr))
}
hgt (t)