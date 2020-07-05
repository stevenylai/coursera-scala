package calculator

object Polynomial extends PolynomialInterface {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal[Double](b() * b() - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal[Set[Double]] {
      val res = Set[Double]()
      if (delta() < 0) res
      else res + ((-b() + math.sqrt(delta())) / 2 / a()) + ((-b() - math.sqrt(delta())) / 2 / a())
    }
  }
}
