package week03
object rationals {
  val x = new Rational(1, 3)
  val y = new Rational(5, 7)
  val z = new Rational(3, 2)

  x + y

  x - y - z

  y + y

  val test = y + y
  test < y
  y < test

  test <= y
  y >= test

  y < y
  y <= y
  y > y
  y >= y
  y == y

  x max y
  y max x

  val xx = x * x
  val yy = y * y

  xx + yy

  x * x + y * y

  class Rational(x: Int, y: Int) {
    require(y != 0, "denominator must be non-zero")
    private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

    val numer = x / gcd(x, y)
    val denom = y / gcd(x, y)

    def this(x: Int) = this(x, 1)

    def + (that: Rational) =
      new Rational(
        numer * that.denom + denom * that.numer,
        denom * that.denom
      )

    def unary_- = new Rational(-numer, denom)

    def - (that: Rational) = this + -that

    def * (that: Rational) =
      new Rational(
        numer * that.numer,
        denom * that.denom
      )

    private def compare(that: Rational, f: (Int, Int) => Boolean) = {
      f(numer * that.denom, that.numer * denom)
    }

    def < (that: Rational) = compare(that, (x, y) => x < y)
    def > (that: Rational) = compare(that, (x, y) => x > y)
    def <= (that: Rational) = compare(that, (x, y) => x <= y)
    def >= (that: Rational) = compare(that, (x, y) => x >= y)
    def == (that: Rational) = compare(that, (x, y) => x == y)

    def max(that: Rational) = if (this < that) that else this

    override def toString: String = numer + "/" + denom
  }
}

