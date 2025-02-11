package funsets

/**
 * This class is a test suite for the methods in object FunSets.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class FunSetSuite extends munit.FunSuite:

  import FunSets.*

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

  trait TestSets:
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s4 = singletonSet(4)
    val s5 = singletonSet(5)
    val s6 = singletonSet(6)
    val s7 = singletonSet(7)

    val u1 = union(s1, s3)
    val u2 = union(s2, singletonSet(4))

    val in1 = intersect(u1, s1)
    val in2 = intersect(u2, s3)

    val dif1 = diff(u1, s3)

    val evenSet = union(u2, s6)

    val mixSet = union(evenSet, s3)

    val isOdd = (arg: Int) => arg % 2 == 1
    val isEven = (arg: Int) => arg % 2 == 0


  /**
   * This test is currently disabled (by using .ignore) because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", remove the
   * .ignore annotation.
   */

    test("singleton set one contains one") {
    
    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets:
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }

  test("union contains all elements of each set") {
    new TestSets:
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
  }

  test("intersect contains only elements from both sets") {
    new TestSets:
      assert(contains(in1, 1), "Intersect 1")
      assert(!contains(in2, 3), "Intersect 2")
  }

  test("diff contains elements from only the first set") {
    new TestSets:
      assert(contains(dif1, 1), "Diff 1")
      assert(!contains(diff(u2, s1), 1), "Diff 2")
  }

  test("forall tests") {
    new TestSets:
      assert(forall(evenSet, isEven), "forall 1")
      assert(!forall(mixSet, isOdd), "forall 2")
  }

  test("exists tests") {
    new TestSets:
      assert(exists(mixSet, isOdd), "Exists 1")
  }

  test("map tests") {
    new TestSets:
      printSet(mixSet)
      printSet(map(mixSet, ((arg: Int) => 2 * arg)))
      assert(forall(map(mixSet, ((arg: Int) => 2 * arg)), isEven), "map doubles even")
  }

  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds
