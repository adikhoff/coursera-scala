package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)

    val s12 = union(s1, s2)
    val s23 = union(s2, s3)
    val s123 = union(s12, s23)

    val empty = intersect(s1, s2)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("union of unions") {
    new TestSets {
      assert(contains(s123, 1), "Union 1")
      assert(contains(s123, 2), "Union 2")
      assert(contains(s123, 3), "Union 3")
    }
  }

  test("intersect contains elements that are in both sets") {
    new TestSets {
      val s = intersect(s12, s23)

      assert(!contains(s, 1), "Intersect 1")
      assert(contains(s, 2), "Intersect 2")
      assert(!contains(s, 3), "Intersect 3")
    }
  }

  test("diff contains elements that are in a but not in b") {
    new TestSets {
      val s = diff(s12, s23)

      assert(contains(s, 1), "Diff 1")
      assert(!contains(s, 2), "Diff 2")
      assert(!contains(s, 3), "Diff 3")
    }
  }

  test("filter contains elements that are in set and satisfy predicate") {
    new TestSets {
      val s = filter(s12, x => x > 1)

      assert(!contains(s, 1), "Diff 1")
      assert(contains(s, 2), "Diff 2")
      assert(!contains(s, 3), "Diff 3")
    }
  }

  test("forall returns true only when all elements in a set satisfy predicate") {
    new TestSets {
      assert(forall(s123, x => x > 0), "x above zero")
      assert(!forall(s123, x => x <= 0), "x below or equal to zero")
      assert(!forall(s123, x => x == 3), "x equals 3")

      assert(forall(empty, x => true), "empty set returns true")
    }
  }

  test("exists returns true when at least one element in a set satisfies predicate") {
    new TestSets {
      assert(exists(s123, x => x > 0), "x above zero")
      assert(!exists(s123, x => x <= 0), "x below or equal to zero")
      assert(exists(s123, x => x == 3), "x equals 3")
    }
  }

  test("map applies a function to existing elements in the list") {
    new TestSets {
      val s = map(s123, x => x * 2)

      assert(!contains(s, 1), "map 1")
      assert(contains(s, 2), "map 2")
      assert(!contains(s, 3), "map 3")
      assert(contains(s, 4), "map 4")
      assert(!contains(s, 5), "map 5")
      assert(contains(s, 6), "map 6")

      assert(!exists(s, x => x < 1))
      assert(!exists(s, x => x > 6))
    }
  }


}
