package calculator

object Polynomial extends PolynomialInterface:
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] =
    Signal {
      (b() * b()) - (4 * a() * c())
    }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] =
    Signal {
      val solutions: Set[Double] = Set()
      if delta() < 0 then solutions
      else {
        val rootOne = (-b() + math.sqrt(delta())) / (2 * a())
        val rootTwo = (-b() - math.sqrt(delta())) / (2 * a())
        solutions + rootTwo + rootOne
      }
    }
