import fpinscala.datastructures.Cons

def mean(xs: Seq[Double]) : Double =
if(xs.isEmpty)
  throw new ArithmeticException("mean of empty list!")
else
  xs.sum / xs.length


def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f
val abs0 : Option[Double] => Option[Double] = lift(math.abs)

//4.3 Using Option
def Try[A](a: => A) : Option[A] =
  try Some(a)
  catch { case e: Exception  => None }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap ( aa => b map (bb => f(aa,bb)))

map2(Some("a"),Some("g"))((x,y) => y.concat(x))
def aFun(a: String,b: String) : String = a.concat(b)

val optionalList = List(Some("Aneesha"), None)
def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
  case aa :: as => aa flatMap( aaa => sequence(as) map (aaa :: _))
  case Nil => Some(Nil)
}

sequence(optionalList)

def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
  a.foldRight[Option[List[B]]](Some(Nil))((x, y) => map2(f(x),y)(_ :: _))

def sequence2[A](a: List[Option[A]]): Option[List[A]] = traverse(a)( b => b)

