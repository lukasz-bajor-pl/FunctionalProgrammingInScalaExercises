import chapter2.exercise1.Fib
import chapter2.exercise2.IsSorted
import org.scalatest.FreeSpec
import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.time.{Millis, Seconds, Span}

/**
  * Created by lbajor on 2016-05-08.
  */
class IsSortedShould extends FreeSpec with TimeLimitedTests {
  val timeLimit = Span(100, Millis)

  "IsSorted.isSorted should" - {

    "return true for empty array" in {
      assert(true === IsSorted.isSorted(Array[Int](), (a: Int, b: Int) => false))
    }

    "return true for one element array" in {
      assert(true === IsSorted.isSorted(Array[Int](1), (a: Int, b: Int) => false))
    }
  }
}
