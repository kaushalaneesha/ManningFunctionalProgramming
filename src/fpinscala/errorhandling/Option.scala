package fpinscala.errorhandling

/**
  * Created by Aneesha on 2/5/2017.
  */
sealed trait Option[+A]{
  def map[B](f: A => B): Option[B] = this match {
    case Some(x) => Some(f(x))
    case None => None
  }
  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(x) => f(x)
    case None => None
  }
  def getOrElse[B >: A](default: => B): B = this match {
    case Some(x) => x
    case None => default
  }
  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }
    def filter(f: A => Boolean): Option[A] =  this match {
      case Some(x) if f(x) => this
      case _ => None
    }

}
case object None extends Option[Nothing]
case class Some[+A](get : A) extends Option[A]