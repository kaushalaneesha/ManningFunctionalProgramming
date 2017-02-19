def square(x: Double,f: => Double): Double = x * x
square(2,sys.error("Hello"))

def maybeTwice(b: Boolean, i: => Int) ={
  lazy val j = i
  if (b) j+j else 0
}

val x = maybeTwice(true, { println("hi"); 1+41 })