package chapter2

import chapter2.exercise4.Uncurry
import org.scalatest.FreeSpec

/**
  * Created by lbajor on 2016-05-08.
  */
class UncurryShould extends FreeSpec {

  "Uncurry.uncurry should" - {

    "compile" in {
      assert(true === Uncurry.uncurry((a: Int) => (s: String) => true)(1, "abc"))
    }
  }
}
