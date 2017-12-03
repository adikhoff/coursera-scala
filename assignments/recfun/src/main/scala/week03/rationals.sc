object rationals {
  val x = new Rational(1, 3)
  val y = new Rational(5, 7)
  val z = new Rational(3, 2)

  x.add(y)

  x.sub(y).sub(z)

  val test = y.add(y)
  test.less(y)
  y.less(test)

  x.max(y)
  y.max(x)



  class Rational(x: Int, y: Int) {
    require(y > 0, "denominator must be above zero")
    private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

    val numer = x / gcd(x, y)
    val denom = y / gcd(x, y)

    def this(x: Int) = this(x, 1)

    def add(that: Rational) =
      new Rational(
        numer * that.denom + denom * that.numer,
        denom * that.denom
      )

    def neg = new Rational(-numer, denom)

    def sub(that: Rational) = add(that.neg)

    def less(that: Rational) = numer * that.denom < that.numer * denom

    def max(that: Rational) = if (this.less(that)) that else this

    override def toString: String = numer + "/" + denom
  }
}

