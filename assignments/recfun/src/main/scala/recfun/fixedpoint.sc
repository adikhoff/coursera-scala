import math.abs

object fixedpoint {

  def tolerance = 0.001

  def closeEnough(x: Double, y: Double): Boolean =
    abs((x - y) / x ) / x < tolerance

  def fixedPoint(f: Double => Double)(firstGuess: Double): Double = {
    def iterate(guess: Double): Double = {
      //println("guess: " + guess)
      val next = f(guess)
      if (closeEnough(next, guess)) guess
      else iterate(next)
    }
    iterate(firstGuess)
  }

  fixedPoint(x => 1 + x / 2)(1)
  def sqrt2(x: Double) = fixedPoint(y => (y + x / y) / 2)(1)

  def sqrt(x: Double) = fixedPoint(averageDamp(y => x / y))(1)

  sqrt2(2)

  sqrt(2)

  sqrt(4)

  def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2



}