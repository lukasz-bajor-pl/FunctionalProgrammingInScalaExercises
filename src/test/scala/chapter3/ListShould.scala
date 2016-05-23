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
      assert(List(2, 3) === List.tail(List(1, 2, 3)))
    }
  }

  "List.setHead should " - {
    "return List(1) for List.setHead(Nil, 1)" in {
      assert(List(1) === List.setHead(Nil, 1))
    }

    "return List(1,2) for List.setHead(List(2,2), 1)" in {
      assert(List(1, 2) === List.setHead(List(2, 2), 1))
    }
  }

  "List.drop should " - {
    "throw UOE on empty list" in {
      intercept[UnsupportedOperationException] {
        List.drop(Nil, 1)
      }
    }

    "return Nil for 1 element list" in {
      assert(Nil === List.drop(List(1), 1))
    }

    "return list without x first elements" in {
      assert(List(3) === List.drop(List(1, 2, 3), 2))
    }
  }

  "List.dropWhile should " - {
    "return Nil on empty list" in {
      assert(Nil === List.dropWhile[Int](Nil, e => true))
    }

    "drop first element on List(1,2) and predicate e <= 1" in {
      assert(List(2) === List.dropWhile(List(1,2), (e:Int) => e <= 1 ))
    }
  }

  "List.init should " - {
    "throw UOE on empty list" in {
      intercept[UnsupportedOperationException] {
        List.init(Nil)
      }
    }

    "return Nil for List.init(List(1)))" in {
      assert(Nil === List.init(List(1)))
    }

    "return List(1) for List.init(List(1,2))" in {
      assert(List(1) === List.init(List(1, 2)))
    }
  }

  "List.productFR should " - {
    "return 1.0 on Nil" in {
      assert(1.0 === List.productFR(Nil))
    }

    "return 2 for List.productFR(List(2)))" in {
      assert(2.0 === List.productFR(List(2)))
    }

    "return 0 for List.productFR(List(2,0,3))" in {
      assert(0.0 === List.productFR(List(2, 0, 3)))
    }
  }

  "3.8 fold right / Cons constructor relation should " - {
    "return Nil on foldRight(Nil, Nil)(new Cons(_,_)" in {
      assert(Nil === List.foldRight(Nil: List[Int], Nil: List[Int])(new Cons(_, _)))
    }

    "return List(1.2.3) on foldRight(List(1,2,3), Nil)(new Cons(_,_)" in {
      assert(List(1, 2, 3) === List.foldRight(List(1, 2, 3), Nil: List[Int])(new Cons(_, _)))
    }
  }

  "3.9 List.lengthFR / lengthFL should " - {
    "return 0 for Nil" in {
      assert(0 === List.lengthFR(Nil))
    }

    "return 3 for List(1.2.3)" in {
      assert(3 === List.lengthFR(List(1, 2, 3)))
    }

    "fail with Stack overflow for List of length 100 000 elements" in {
      intercept[StackOverflowError] {
        List.lengthFR(List((1 to 100000): _*))
      }
    }

    "lengthFL should not fail with Stack overflow for List of length 100 000 elements" in {
      assert(100000 === List.lengthFL(List((1 to 100000): _*)))
    }
  }

  "3.7 short circuiting using lazy foldright should " - {
    "work on very long sequence when calculating product when one of elements is 0" in {
      val longTail = List((1 to 200000).map(_.toDouble): _*)
      assert(0.0 === List.lazyFoldRight(new Cons(0.0, longTail), 1.0)((el, z) => if (el == 0.0) 0.0 else z))
    }
  }
}
