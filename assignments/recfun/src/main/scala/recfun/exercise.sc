object exercise {
  def product(f: Int => Int)(a: Int, b: Int) : Int =
    mapReduce(f, (x, y) => x * y, 1)(a, b)

  def sum(f: Int => Int)(a: Int, b: Int) : Int =
    mapReduce(f, (x, y) => x + y, 0)(a, b)

  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)
               (a: Int, b: Int): Int = {
    println("a:" + a + " b:" + b)
    if (a > b) zero
    else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))
  }

  def factorial(n: Int) : Int = {
    product(x => x)(1, n)
  }

  def id(x: Int) = x

  def productId = product(x => x)_

  productId(1, 4)

  product(x => x)(1, 4)

  factorial(3)

  mapReduce(x => x, (x, y) => x * y, 1)(1, 4)

  mapReduce(x => x, (x, y) => x + y, 0)(1, 4)

  product(x => x)(1, 4)
  sum(x => x)(1, 4)

}