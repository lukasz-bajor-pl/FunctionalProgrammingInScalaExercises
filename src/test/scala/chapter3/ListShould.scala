package chapter3

import org.scalatest.{FreeSpec}

/**
  * Created by lbajor on 2016-05-17.
  */
class ListShould extends FreeSpec {

  "List.tail should " - {
    "throw UOE on empty list" in {
      intercept[UnsupportedOperationException] {
        List.tail(Nil)
      }
    }

    "return Nil for 1 element list" in {
      assert(Nil === List.tail(List(1)))
    }

    "return list without 1st element" in {
      assert(List(2,3) === List.tail(List(1,2,3)))
    }
  }
}
