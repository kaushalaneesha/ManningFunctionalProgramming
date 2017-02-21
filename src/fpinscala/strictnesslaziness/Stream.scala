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

  def takeWhile_1(p: A=> Boolean) : Stream[A] = foldRight(empty[A])((x,y) => if (p(x)) cons(x,y) else empty)

  def headOption_1 : Option[A] = foldRight(None : Option[A])((h,_) => Some(h))

  def map[B](f: A => B) : Stream[B] = foldRight(empty[B])((x,y) => cons(f(x),y))

  def filter(f: A => Boolean) : Stream[A] = foldRight(empty[A])((h,t) => if(f(h)) cons(h,t) else t)

  def flatMap[B](f: A => Stream[B]) : Stream[B] = foldRight(empty[B])((h,t) => f(h) append t)

  def append[B >: A](z: => Stream[B]): Stream[B] = foldRight(z)((h, t) => cons(h,t))

  def constant[A](a: A): Stream[A] = cons(a,constant(a))
  //or
  def constant_1[A](a:A) : Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  def from(n: Int): Stream[Int] = cons(n,from(n+1))

  def fibs(a: Int, b: Int) : Stream[Int] = cons(a,fibs(b,a+b))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => empty
    case Some((a,s)) => cons(a,unfold(s)(f))
  }

  def fibsViaUnfold(a: Int, b: Int) : Stream[Int] = unfold(0,1){ case (x,y) => Some(x,(y,x+y)) }
  def fromViaUnflod(n:Int) : Stream[Int] = unfold(n)(n => Some(n,n+1))
  def constantViaUnfold[A](a:A) : Stream[A] = unfold(a)(a => Some(a,a))

  def mapViaUnfold[B](f : A => B) : Stream[B] =
   unfold(this) {
     case Cons(h, t) => Some((f(h()), t()))
     case _          => None
  }
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