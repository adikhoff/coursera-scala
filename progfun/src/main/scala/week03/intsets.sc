object intsets {
  println("hello")

  val fullSet = Empty.incl(5).incl(3).incl(2).incl(4).incl(8).incl(7)
  val otherSet = Empty.incl(2).incl(100).incl(10).incl(20).incl(5)

  fullSet
  fullSet.contains(3)
  fullSet.contains(-1)

  fullSet union otherSet

  abstract class IntSet {
    def incl(x: Int): IntSet
    def contains(x: Int): Boolean
    def union(other: IntSet): IntSet
  }

  object Empty extends IntSet {
    def incl(x: Int) = new NonEmpty(x, Empty, Empty)
    def contains(x: Int) = false
    def union(other: IntSet) = other

    override def toString: String = "."
  }

  class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
    def incl(x: Int) =
      if (x < elem) new NonEmpty(elem, left incl x, right)
      else if (x > elem) new NonEmpty(elem, left, right incl x)
      else this

    def contains(x: Int) = {
      if (x < elem) left contains x
      else if (x > elem) right contains x
      else true // x == value
    }

    def union(other: IntSet) =
      ((left union right) union other) incl elem

    override def toString: String = "{" + left + elem + right + "}"
  }

}


