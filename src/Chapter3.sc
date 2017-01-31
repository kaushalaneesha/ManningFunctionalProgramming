import fpinscala.datastructures.{Cons, List,Nil}

val x = List(1,2,3,4,5) match {
  case Cons(x, Cons(2, Cons(4, _))) => x
  case Nil => 42
  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
  case Cons(h, t) => h + List.sum(t)
  case _ => 101
}
val strList = List("Aneesha", "Shashwat", "Pulkit")
List.drop(strList,1)
List.tail(Nil)
List.setHead(strList, "Butter")
List.setHead(Nil, "Butter")
