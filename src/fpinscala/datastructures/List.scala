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

  def tail[A](ds : List[A]) : List[A] = ds match {
    case Nil => Nil
    case Cons(x,xs) => xs
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def dropWhile[A](l: List[A])( f: A => Boolean): List[A] = l match {
    case Cons(x,xs) if f(x) => dropWhile(xs)(f)
    case _ => l
  }

  def foldRight[A,B](as : List[A], z : B) (f : (A,B) => B) : B =
    as match {
      case Nil => z
      case Cons(x,xs) => f(x,foldRight(xs,z)(f))
    }
  //Ex 3.9
  def length[A](as : List[A]) : Int = foldRight(as,0)((_,y) => y+1)
  //Ex 3.10
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x,xs) => foldLeft(xs,f(z,x))(f)
  }
  //Ex 3.11
  def sumLeft(xs: List[Int]) : Int = foldLeft(xs,0)(_+_)
  def prodLeft(xs : List[Double]) : Double = foldLeft(xs,1.0)((x,y) => if(y==0.0) 0.0 else x*y)
  //Ex 3.12
  def reverse[A](xs: List[A]) : List[A] = foldLeft(xs,Nil: List[A])((x,y) => Cons(y,x))
  //Ex: 3.13
  def foldLeft2[A,B](as: List[A],z:B)(f:(B,A) => B) : B = foldRight(as,z)((x,y) => f(y,x))
  def foldRight2[A,B](as: List[A],z:B)(f:(A,B) => B) : B = foldLeft(as,z)((x,y) => f(y,x))
  //Ex: 3.14
  def append[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1,a2)(Cons(_,_))
  //Ex: 3.15
  def concat[A](a : List[List[A]]) : List[A] = a match {
    case Cons(x,Cons(y,xs)) => concat(Cons(append(x,y),xs))
    case Cons(x,Nil) => x
    case Nil => Nil
  }
  //Ex 3.16
  def addOne(xs : List[Int]) : List[Int] = foldRight(xs,Nil : List[Int])((x,y) => Cons(x+1,y))
  //Ex 3.17
  def doubleToString(xs : List[Double]) : List[String] = foldRight(xs,Nil : List[String])((x,y) => Cons(x.toString,y))
  //Ex 3.18
  def map[A,B](as: List[A])(f: A => B): List[B] = foldRight(as,Nil : List[B])((x,y) => Cons(f(x),y))
  //Ex 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRight(as,Nil : List[A])((x,y) => if(f(x)) Cons(x,y) else y)
  //Ex 3.20
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = concat(map(as)(f))
  //Ex 3.21
  def filterFlatMap[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(a => if(f(a)) List(a) else List())
  //Ex 3.22
   def addLists(xs: List[Int], as : List[Int]) : List[Int] = (xs,as) match {
     case (Cons(x,txs), Cons(y,tys)) => Cons(x+y,addLists(txs,tys))
     case (Nil,Nil) => Nil
   }
  //Ex 3.23
  def zipWith[A,B](xs: List[A], as : List[A])(f : (A,A) => B) : List[B] = (xs,as) match {
    case (Cons(x,txs), Cons(y,tys)) => Cons(f(x,y),zipWith(txs,tys)(f))
    case (Nil,Nil) => Nil
  }
  //Ex 3.24
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = if(sup == Nil) false else sub match {
    case Nil => true
    case Cons(x,xs) => hasSubsequence(tail(dropWhile(sup)(!_.equals(x))),xs)
  }
}
