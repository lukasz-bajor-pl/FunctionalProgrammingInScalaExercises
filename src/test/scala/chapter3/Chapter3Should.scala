package chapter3

import org.scalatest.{FreeSpec, FunSuite}

/**
  * Created by lbajor on 2016-05-17.
  */
class Chapter3Should extends FreeSpec {

  "Chapter 3" - {
    "guessing it should return 3 as exercise 1" in {
      assert(3 === Chapter3.exercise1())
    }
  }
}
