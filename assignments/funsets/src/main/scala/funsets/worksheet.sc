package funsets

object worksheet {
  import FunSets._

  val s1 = singletonSet(1)
  val s2 = singletonSet(2)
  val s3 = singletonSet(3)

  val s12 = union(s1, s2)
  val s23 = union(s1, s2)

  println(s23)
  forall(union(s12, s23), x => true)




}