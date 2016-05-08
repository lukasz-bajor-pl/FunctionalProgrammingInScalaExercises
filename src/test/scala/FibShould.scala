import chapter2.exercise1.Fib
import org.scalatest.FreeSpec

/**
  * Created by lbajor on 2016-05-08.
  */
class FibShould extends FreeSpec {
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
  }
}
