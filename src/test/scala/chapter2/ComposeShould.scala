package chapter2

import chapter2.exercise5.Compose
import org.scalatest.FreeSpec

/**
  * Created by lbajor on 2016-05-08.
  */
class ComposeShould extends FreeSpec {

  "Compose.compose should" - {

    "apply both functions" in {
      assert("126" === Compose.compose((b: Int) => b.toString, (a: Int) => 2*a)(63))
    }
  }
}
