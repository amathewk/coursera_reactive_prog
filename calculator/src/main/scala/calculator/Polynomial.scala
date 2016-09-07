package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal (
      Math.pow(b(), 2) - 4 * a() * c()
    )
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    var sols : Set[Double] = Set()
    val deltaVal = delta()
    val aVal = a()
    val bVal = b()
    val cVal = c()
    if ( deltaVal < 0 ) {
      return Signal(sols)
    } else {
      val sqDelta = Math.sqrt(deltaVal)
      val x1: Double = (-bVal + sqDelta) / 2 * aVal
      val x2: Double = (-bVal - sqDelta) / 2 * aVal
      println (s"${x1}, ${x2}")
      sols = sols + x1
      sols = sols + x2
      println(sols)
      return Signal(sols)
    }
  }
}
