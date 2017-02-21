import fpinscala.strictnesslaziness._
Stream(1,2,3).take(2).toList

Stream(4,2,3,4,6).takeWhile_1(x => x%2==0 ).toList

val rng = new scala.util.Random

rng.nextDouble
rng.nextDouble
rng.nextInt(10)

def rollDie: Int = {
  val rng = new scala.util.Random
  rng.nextInt(6)
}