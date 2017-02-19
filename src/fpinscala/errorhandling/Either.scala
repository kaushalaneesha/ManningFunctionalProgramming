package fpinscala.errorhandling

/**
  * Created by Aneesha on 2/18/2017.
  */
trait Either[+E,+A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(a) => Right(f(a))
    case Left(e) => Left(e)
  }
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => f(a)
    case Left(e) => Left(e)
  }
  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => Right(a)
    case Left(_) => b
  }
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = for {
    bb <- b
    aa <- this
  }yield f(aa,bb)
  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]]  =
    as.foldRight[Either[E, List[B]]](Right(Nil))((x,y) => f(x).map2(y)(_ :: _))
  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] = traverse(es)( a => a)
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]