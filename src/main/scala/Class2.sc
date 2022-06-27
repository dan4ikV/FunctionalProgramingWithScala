import scala.math._

def binaryReverse(num: Int): Int = num match {
  case 0 => 0
  case 1 => 1
  case _ =>
    if ((num & 1) == 1) {
      ((binaryReverse(num >> 1) >> 1) | Math.pow(2, num.toBinaryString.length).toInt)
    }
    else {
      (binaryReverse(num >> 1) >> 1)
    }
}

def binaryReverseTailRec(num: Int): Int = {
  def go(num: Int, accum: Int): Int = num match {
    case 0 =>
      accum
    case _ =>
      go(num >> 1, (accum << 1) | num & 1)
  }
  go(num, 0)
}

def sqrtCurry(tolerance: Int, startingPoint: Int, number: Int) =
  tolerance * startingPoint * 2
binaryReverse(3)



def sqrtCurry(eps: Double)(x0: Double)(a: Double): Double = {
  def sqrtCurryHelper(xn: Double): Double = {
    val xn1 = 0.5 * (xn + a / xn)
    if(abs(xn1 - xn) < eps) xn1 else sqrtCurryHelper(xn1)
  }

  sqrtCurryHelper(x0)
}

val sqrtParametrized = sqrtCurry(0.001)(100.0)(_)

sqrtParametrized(0.0)
sqrtParametrized(1.0)
sqrtParametrized(2.0)
sqrtParametrized(4.0)