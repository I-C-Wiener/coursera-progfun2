package calculator

import Math.sqrt

object Polynomial {

  // Δ = b² - 4ac
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Var(b() * b() - 4 * a() * c())
  }

  // (-b ± √Δ) / 2a
  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {

    Var(delta() match {
      case d if d < 0 => Set()
      case 0 => Set(-b() / (2 * a()))
      case d => Set((-b() + sqrt(d)) / (2 * a()), (-b() - sqrt(d) / (2 * a())))
    })
  }
}
