package fpinscala.strictnesslaziness

/**
  * Created by Aneesha on 2/18/2017.
  */
import Stream._

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h,t) => h() :: t().toList
  }
  def take(n : Int) : Stream[A] = this match {
    case Cons(h,t) if n > 1 => cons(h(),t().take(n-1))
    case Cons(h,_) if n == 1 => cons[A](h(),empty)
    case _ => empty
  }
  def drop(n:Int) :Stream[A]  = this match {
    case Cons(_,t) if n > 0 => t().drop(n-1)
    case _ => this
  }
  def takeWhile(p: A=> Boolean) : Stream[A] = this match {
    case Cons(h,t) if p(h()) => cons(h(),t().takeWhile(p))
    case _ => empty
  }
  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }
  def foldRight[B](z: => B)(f: (A, => B) => B) : B = this match {
    case Cons(h,t) => f(h(),t().foldRight(z)(f))
    case _ => z
  }
  def exists_1(p: A => Boolean): Boolean = foldRight(false)(p(_) || _)

  def forAll(p: A => Boolean): Boolean = foldRight(true)(p(_) && _)

  
}
case object Empty extends Stream[Nothing]
case class Cons[+A] ( h : () => A, t :()=> Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, t1 : => Stream[A]) : Stream[A] = {
    lazy val head = hd
    lazy val tail = t1
    //Due to technical limitations, these are thunks that must be explicitly forced, rather than by-name parameters
    Cons(() => head,() => tail)
  }
  def empty[A] : Stream[A] = Empty

  def apply[A](as : A*) : Stream[A] = if(as.isEmpty) empty else cons(as.head,apply(as.tail: _*))

}