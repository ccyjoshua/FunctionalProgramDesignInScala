package calculator

object Polynomial extends PolynomialInterface:
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] =
    Signal {
      val va = a()
      val vb = b()
      val vc = c()
      vb * vb - 4 * va * vc
    }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] =
    Signal {
      val va = a()
      val vb = b()
      val vc = c()
      val vd = delta()
      if vd < 0 then Set()
      else if vd == 0 then Set(-1 * vb / 2 / va)
      else Set((-1 * vb + Math.sqrt(vd)) / 2 / va, (-1 * vb - Math.sqrt(vd)) / 2 / va)
    }
