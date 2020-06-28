package funsets

object Main extends App {
  import FunSets._
  println(contains(singletonSet(1), 1))
  val even = (i: Int) => i % 2 == 0
  val odd = (i: Int) => i % 2 != 0
  def greater(threshold: Int)(i: Int): Boolean = (i > threshold)
  println(FunSets.toString(filter(even, greater(992))))
  println(FunSets.forall(even, odd))
  println(FunSets.forall(even, even))
  println(FunSets.forall(even, singletonSet(2)))
  println(FunSets.exists(even, odd))
}
