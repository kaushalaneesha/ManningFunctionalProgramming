package fpinscala.datastructures

/**
  * Created by Aneesha on 1/29/2017.
  */

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head : A, tail : List[A]) extends List[A]
object List {
  def drop[A](l: List[A], n: Int) : List[A] = if (n<=0) l else l match {
    case Nil => Nil
    case Cons(x,xs) => drop(xs,n-1)
  }
  def setHead(ds : List[String], str : String) : List[String] = ds match {
    case Nil => Cons(str,Nil)
    case Cons(x,xs) => Cons(str,xs)
  }

  def sum(ints : List[Int]) : Int = ints match{
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds : List[Double]) : Double = ds match {
    case Nil => 1
    case Cons(0.0,_) => 0
    case Cons(x,xs) => x * product(xs)
  }

  def tail(ds : List[Int]) : List[Int] = ds match {
    case Nil => Nil
    case Cons(x,xs) => xs
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}
