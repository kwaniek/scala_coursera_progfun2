package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double], c: Signal[Double]): Signal[Double] = {
    Var(b() * b() - 4.0 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double], c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Var(
      if (delta() > 0 && a() != 0)
        Set(
          (-b() + Math.sqrt(delta())) / (2 * a()),
          (-b() - Math.sqrt(delta())) / (2 * a())
        )
      else
        Set()
    )
  }
}
