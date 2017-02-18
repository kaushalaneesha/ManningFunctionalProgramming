/**
  * Created by Aneesha on 2/5/2017.
  */
sealed trait Option[+A]{
  def map[B](f: A => B): Option[B] = Some(f(this))
  def flatMap[B](f: A => Option[B]): Option[B] = f(this)
  def getOrElse[B >: A](default: => B): B = this match {
    case Some(x) => x
    case None => default
  }
  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None =>ob
    case _ => _
  }
    def filter(f: A => Boolean): Option[A] =  if(f(this)) this else None
}
case class Some[+A] (get : A) extends Option[A]
case object None extends Option[Nothing]