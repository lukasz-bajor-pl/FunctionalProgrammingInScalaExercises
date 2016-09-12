package pers.fpinscala.chapter2

import pers.fpinscala.chapter2.exercise3.Curry
import org.scalatest.FreeSpec

/**
  * Created by lbajor on 2016-05-08.
  */
class CurryShould extends FreeSpec {

  "Curry.curry should" - {

    "compile" in {
      assert(true === Curry.curry((a: Int, b: String) => true)(1)("abc"))
    }
  }
}
