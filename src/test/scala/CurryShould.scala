import chapter2.exercise2.IsSorted
import chapter2.exercise3.Curry
import org.scalatest.FreeSpec
import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.time.{Millis, Span}

/**
  * Created by lbajor on 2016-05-08.
  */
class CurryShould extends FreeSpec {

  "Curry.curry should" - {

    "compile" in {
      Curry.curry((a: Int, b: String) => true)
    }
  }
}
