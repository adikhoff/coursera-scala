package week04

trait List[T] {
  def head: T
  def tail: List[T]

  def push(x: T) = new Cons(x, this)
  def add(x: T):List[T]
  def isEmpty: Boolean
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def add(x: T): List[T] = {
    new Cons[T](head, tail.add(x))
  }

  override def isEmpty = false
  override def toString = head + " " + tail
}

class Nil[T] extends List[T] {
  override def head: Nothing = throw new NoSuchElementException("Nil.head")
  override def tail: Nothing = throw new NoSuchElementException("Nil.tail")

  def add(x: T) = new Cons(x, new Nil[T])

  override def isEmpty = true
  override def toString = "Nil"
}
