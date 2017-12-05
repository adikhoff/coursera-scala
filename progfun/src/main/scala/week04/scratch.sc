import week04._

object scratch {
  val top = new Nil[Int]

  val list = top push 1 push 2 push 3
  val list2 = top add 1 add 2 add 3

  val list3 = new Nil[String] add "a" add "b"

  val list4 = new Nil[Any] add "a" add "b" add 1 add 2 add 3

  val xx = nth(0, list4)

  def nth[T](n: Int, list: List[T]): T = {
    if (list.isEmpty) throw new IndexOutOfBoundsException()
    else if (n == 0) list.head
    else nth(n - 1, list.tail)
  }
}

