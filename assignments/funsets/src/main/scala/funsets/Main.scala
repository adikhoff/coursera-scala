package funsets

object Main extends App {
  import FunSets._
  println(contains(singletonSet(1), 1))
  println(contains(singletonSet(2), 1))

  val s1 = singletonSet(1)
  val s2 = singletonSet(2)
  val s3 = singletonSet(3)

  val s12 = union(s1, s2)
  val s23 = union(s1, s2)
  val s123 = union(s12, s23)

  println(forall(s123, x => x > 0))
  println(forall(s123, x => x == 3))


}
