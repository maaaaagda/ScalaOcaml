val list1 = 1::2::3::Nil
val list2 = 4::5::Nil
val list3 = 6::list1::8::Nil
def silnia(n:Int):Int =
 if (n==0) 1 else n*silnia(n-1)
silnia(4)
 
 def flatten[A](listOfLists: List[List[A]]): List[A] = {
	if (listOfLists == Nil) Nil
	else listOfLists.head ::: flatten(listOfLists.tail)
}

flatten(list2)