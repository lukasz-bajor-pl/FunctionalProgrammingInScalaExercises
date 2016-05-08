import chapter2.exercise1.Fib
import org.scalatest.FreeSpec
import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.time.{Millis, Seconds, Span}

/**
  * Created by lbajor on 2016-05-08.
  */
class IsSorted extends FreeSpec with TimeLimitedTests {
  val timeLimit = Span(100, Millis)

  "IsSorted.isSorted should" - {
  }
}
