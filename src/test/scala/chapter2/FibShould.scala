package chapter2

import chapter2.exercise1.Fib
import org.scalatest.FreeSpec
import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.time.{Seconds, Span}

/**
  * Created by lbajor on 2016-05-08.
  */
class FibShould extends FreeSpec with TimeLimitedTests {
  val timeLimit = Span(1, Seconds)

  "Fib.fib should" - {
    "return 0 as first fibonacci number" in {
      assert( 0 === Fib.fib(0) )
    }

    "return 1 as second fibonacci number" in {
      assert( 1 === Fib.fib(1) )
    }

    "return 1 as 2nd fibonacci number" in {
      assert( 1 === Fib.fib(2) )
    }

    "return 2 as 3rd fibonacci number" in {
      assert( 2 === Fib.fib(3) )
    }

    "return 3 as 4th fibonacci number" in {
      assert( 3 === Fib.fib(4) )
    }

    "use tail recursion to quickly return 1134903170 as 45th fibonacci number" in {
      assert(1134903170 === Fib.fib(45))
    }
  }
}
