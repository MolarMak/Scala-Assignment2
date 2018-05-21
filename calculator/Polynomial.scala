package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double], c: Signal[Double]): Signal[Double] = Signal(Math.pow(b(), 2) - 4*(a() * c()))

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
      val delta = computeDelta(a, b, c)()
      if (delta < 0) {
        Set[Double]()
      } else if (delta == 0) {
        val answer = -b() / (2 * a())
        Set[Double](answer)
      } else {
        val answer = formula(a(), b(), c())
        Set[Double](answer._1, answer._2)
      }
    }
  }

  def formula(a: Double, b: Double, c: Double): (Double, Double) = {
    val delta = Math.pow(b, 2) - 4 * a * c
    ((-b + delta) / (2 * a), (-b - delta) / (2 * a))
  }
}
