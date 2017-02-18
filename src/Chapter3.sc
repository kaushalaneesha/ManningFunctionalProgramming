import fpinscala.datastructures.{Cons, List,Nil}
import fpinscala.datastructures.List._

val x = List(1,2,3,4,5) match {
  case Cons(x, Cons(2, Cons(4, _))) => x
  case Nil => 42
  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
  case Cons(h, t) => h + sum(t)
  case _ => 101
}
val strList = List("Aneesha", "Shashwat", "Pulkit")
drop(strList,1)
tail(Nil)
setHead(strList, "Butter")
setHead(Nil, "Butter")
dropWhile(strList)( x => x!="Mumma")

val xs: List[Int] = List(1,2,3,4,5)
val xs2: List[Double] = List(1,2,3,4,5)
val ex1 = dropWhile(xs)(x => x < 4)

//Ex 3.7
foldRight(xs,1.0)((x,y) => if(x==0.0) 0.0 else x * y)
//Ex 3.8
foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
//Ex 3.9
length(strList)
length(xs)
length(Nil)
//Ex 3.10
val smallList = List(1,2,3)
foldLeft(smallList,10)(_ - _)
//Ex 3.11
sumLeft(xs)
prodLeft(xs2)
//Ex : 3.12
reverse(xs)
//Ex : 3.13
foldLeft2(smallList,10)(_ - _)
foldRight2(smallList,10)(_ - _)

//Ex 3.14
val a2 = List(7,8,9)
append(xs,a2)
//Ex 3.15
val a3 = List(10,11)
val ll = List(xs,a2,a3)
concat(ll)
//Ex 3.17
addOne(xs)
addOne(Nil)
//Ex 3.18
val doubleList = List(1.0,2.0,3.0)
doubleToString(doubleList)
//Ex 19
filter(xs)(_%2==0)
//Ex 20
flatMap(List(1,2,3))(i => List(i,i))
//Ex 22
val l1 = List(1,2,3)
val l2 = List(4,5,6)
addLists(l1,l2)
hasSubsequence(List(1,2,3,4),List(4))