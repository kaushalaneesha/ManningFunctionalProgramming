package fpinscala.datastructures

/**
  * Created by Aneesha on 2/1/2017.
  */
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](tr : Tree[A]) : Int = tr match {
    case Leaf(_) => 1
    case Branch(ltr,rtr) => size(ltr) + size(rtr)
  }

  def max(tr : Tree[Int]) : Int = tr match {
    case Leaf(v) => v
    case Branch(ltr,rtr) => max(ltr).max(max(rtr))
  }
}
