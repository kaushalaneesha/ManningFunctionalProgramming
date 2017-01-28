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

findFirst(Array(1,3,4,9,3,9), x => x==9)
isSorted(Array(4),ordered)

def ordered(a:Int, b:Int) = if(a<=b) true else false