package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    val bv = b()
    Signal(bv*bv-4*(a()*c()))
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
      if (delta() < 0) Set()
      else if (delta() == 0) Set(-b() / (2 * a()))
      else {
        val x1 = (-b() + Math.sqrt(delta())) / (2 * a())
        val x2 = (-b() - Math.sqrt(delta())) / (2 * a())
        Set(x1, x2)
      }
    }
  }
}
