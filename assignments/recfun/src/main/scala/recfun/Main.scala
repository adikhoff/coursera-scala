package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println("Balancing parantheses")
    val parstrings = List("(if (zero? x) max (/ 1 x))", "I told him (that it’s not (yet) done). (But he wasn’t listening)", ":-)", "())(")

    for (s <- parstrings) {
      println(s + " " + balance(s.toList))
    }

    println("Counting change")
    println("4 (1,2): " + countChange(4, List(1, 2)))
    println("4 (1,2,3): " + countChange(4, List(1, 2, 3)))
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (r == 0) 1
    else {
      val cl = if (c == 0) 0 else pascal(c - 1, r - 1)
      val cr = if (c >= r) 0 else pascal(c, r - 1)
      cl + cr
    }
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def travel(chars: List[Char], level: Int): Boolean = {
      if (level < 0) false
      else if (chars.isEmpty) level == 0
      else if (chars.head == '(') travel(chars.tail, level + 1)
      else if (chars.head == ')') travel(chars.tail, level - 1)
      else travel(chars.tail, level)
    }

    travel(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (coins.isEmpty) 0
    else if (money < 0) 0
    else if (money == 0) 1
    else {
      countChange(money - coins.head, coins) +
        countChange(money, coins.tail)
    }
  }
}
