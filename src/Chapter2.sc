def findFirst[A](arr : Array[A], p : A=> Boolean) : Int = {
  @annotation.tailrec
  def loop(n : Int) : Int =
    if(n>=arr.length) -1
    else if(p(arr(n))) n
    else loop(n+1)
  loop(0)
}

def isSorted[A](as: Array[A], ordered : (A,A) => Boolean) : Boolean ={
  def loop(n: Int) : Boolean = {
    if(n>=as.length-1) true
    else if(ordered(as(n),as(n+1))) loop(n+1)
    else false
  }
  loop(0)
}

def partial1[A,B,C](a: A, f: (A,B)=>C) : B=>C = (b:B) => f(a,b)
//Ex 2.3
def curry[A,B,C](f: (A,B) => C): A => (B => C) = a => b => f(a,b)
//Ex 2.4
def uncurry[A,B,C](f: A=>B=>C): (A,B) => C = (a,b) => f(a)(b)
//Ex 2.5
def compose[A,B,C](f: B=> C, g: A=>B) : A => C = a => f(g(a))
findFirst(Array(1,3,4,9,3,9), x => x==9)
isSorted(Array(4),ordered)

def ordered(a:Int, b:Int) = if(a<=b) true else false