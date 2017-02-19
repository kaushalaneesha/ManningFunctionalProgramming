import fpinscala.datastructures.Cons
import fpinscala.errorhandling

def mean(xs: Seq[Double]) : Double =
if(xs.isEmpty)
  throw new ArithmeticException("mean of empty list!")
else
  xs.sum / xs.length


def lift[A,B](f: A => B): errorhandling.Option[A] => errorhandling.Option[B] = _ map f
val abs0 : errorhandling.Option[Double] => errorhandling.Option[Double] = lift(math.abs)

//4.3 Using fpinscala.errorhandling.Option
def Try[A](a: => A) : errorhandling.Option[A] =
  try errorhandling.Some(a)
  catch { case e: Exception  => errorhandling.None }

  def map2[A,B,C](a: errorhandling.Option[A], b: errorhandling.Option[B])(f: (A, B) => C): errorhandling.Option[C] =
    a flatMap ( aa => b map (bb => f(aa,bb)))

map2(errorhandling.Some("a"),errorhandling.Some("g"))((x, y) => y.concat(x))
def aFun(a: String,b: String) : String = a.concat(b)

val optionalList = List(errorhandling.Some("Aneesha"), errorhandling.None)
def sequence[A](a: List[errorhandling.Option[A]]): errorhandling.Option[List[A]] = a match {
  case aa :: as => aa flatMap( aaa => sequence(as) map (aaa :: _))
  case Nil => errorhandling.Some(Nil)
}

sequence(optionalList)

def traverse[A, B](a: List[A])(f: A => errorhandling.Option[B]): errorhandling.Option[List[B]] =
  a.foldRight[errorhandling.Option[List[B]]](errorhandling.Some(Nil))((x, y) => map2(f(x),y)(_ :: _))

def sequence2[A](a: List[errorhandling.Option[A]]): errorhandling.Option[List[A]] = traverse(a)(b => b)

def map2UsingFor[A,B,C](a: errorhandling.Option[A], b: errorhandling.Option[B])(f: (A, B) => C): errorhandling.Option[C] =
  for {
  aa <- a
  bb <- b
}yield f(aa,bb)


