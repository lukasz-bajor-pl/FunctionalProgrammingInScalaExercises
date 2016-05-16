package chapter2

import chapter2.exercise2.IsSorted
import org.scalatest.FreeSpec
import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.time.{Millis, Span}

/**
  * Created by lbajor on 2016-05-08.
  */
class IsSortedShould extends FreeSpec with TimeLimitedTests {
  val timeLimit = Span(600, Millis)

  "IsSorted.isSorted should" - {

    "return true for empty array" in {
      assert(true === IsSorted.isSorted(Array[Int](), (a: Int, b: Int) => false))
    }

    "return true for one element array" in {
      assert(true === IsSorted.isSorted(Array[Int](1), (a: Int, b: Int) => false))
    }

    "return false for 2 el array with function returning false" in {
      assert(false === IsSorted.isSorted(Array[Int](1, 2), (a: Int, b: Int) => false))
    }

    "return true for 2 el array with function returning true" in {
      assert(true === IsSorted.isSorted(Array[Int](1, 2), (a: Int, b: Int) => true))
    }

    "return false for 3 el unsorted array with proper comp function" in {
      assert(false === IsSorted.isSorted(Array[Int](1, 2, 1), (a: Int, b: Int) => a < b))
    }

    "return true for 10000 el sorted array with proper comp function" in {
      val ar = (1 to 10000).toArray

      assert(true === IsSorted.isSorted(ar, (a: Int, b: Int) => a < b))

    }
  }
}
