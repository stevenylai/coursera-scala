import scala.annotation.tailrec

object session {
  println("Welcome")
  def mysqrt(x: Double): Double = {
    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))

    def isGoodEnough(guess: Double): Boolean =
      Math.abs(
        Math.max(guess * guess, x) /
          Math.min(guess * guess, x) - 1) < 0.001

    def improve(guess: Double): Double =
      (x / guess + guess) / 2
    sqrtIter(1.0)
  }

  mysqrt(2)
  mysqrt(0.001)
  mysqrt(0.1e-20)
  mysqrt(1e20)
  mysqrt(1e50)

  def factorial(num: Int): Int = {
    @tailrec
    def doFactorial(num: Int, res: Int): Int = {
      if (num == 0) res else doFactorial(num - 1, res * num)
    }
    doFactorial(num, 1)
  }
  factorial(6)
}