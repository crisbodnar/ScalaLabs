package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    new Signal(b() * b() - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    new Signal(
      if(a() == 0)
        Set(-c() / b())
      else if(delta() < 0)
        Set()
      else {
        val x1 = (b() + Math.sqrt(delta())) / 2 / a()
        val x2 = (b() - Math.sqrt(delta())) / 2 / a()
        Set(x1, x2)
      }
    )
  }
}
