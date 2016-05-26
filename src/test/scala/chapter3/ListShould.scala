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

  "3.11 sum / product / length as fold left should " - {
    "sumFL return 0 for Nil" in {
      assert(0 === List.sumFL(Nil: List[Int]))
    }

    "sumFL return 6 for List(1,2,3)" in {
      assert(6 === List.sumFL(List(1,2,3)))
    }

    "productFL return 1.0 for Nil" in {
      assert(1.0 === List.productFL(Nil: List[Double]))
    }

    "productFL return 24 for List(1,2,3,4)" in {
      assert(24 === List.productFL(List(1.0,2.0,3.0,4.0)))
    }
  }

  "3.12 reverse as fold left should " - {
    "return Nil for Nil" in {
      assert(Nil === List.reverseFL(Nil))
    }

    "return List(3,2,1) for List(1,2,3)" in {
      assert(List(3,2,1) === List.reverseFL(List(1,2,3)))
    }
  }

  "3.13 foldLeftAsFR should" - {
    "reverse list" in {
      assert(List(3,2,1) === List.foldLeftAsFR(List(1,2,3), Nil: List[Int])((z, el) => new Cons(el, z)))
    }
  }

  "3.13 foldRightAsFL should" - {
    "not reverse list" in {
      assert(List(1,2,3) === List.foldRightAsFL(List(1,2,3), Nil: List[Int])((el, z) => new Cons(el, z)))
    }

    "foldRightAsFL is tail recursive" in {
      List.foldRightAsFL(List((1 to 100000): _*), 0)((el, z) => z+1)
    }
  }

  "3.14 append should" - {
    "return List(1) after appending 1 to Nil" in {
      assert(List(1) === List.append(Nil: List[Int], 1))
    }

    "return List(1, 2) after appending 2 to List(1)" in {
      assert(List(1,2) === List.append(List(1), 2))
    }
  }

  "3.15 concatenate lists should" - {
    "return Nil for Nil" in {
      assert(Nil === List.concatenate(Nil: List[List[Int]]))
    }

    "return List(1,2,3) for List(List(1,2),List(3))" in {
      assert(List(1,2,3) === List.concatenate(List(List(1,2), List(3))))
    }
  }

  "3.16 increase should" - {
    "return Nil for Nil" in {
      assert(Nil === List.increase(Nil, 1))
    }

    "return List(3,4,5) for List(1,2,3) and 2" in {
      assert(List(3,4,5) === List.increase(List(1,2,3), 2))
    }
  }

  "3.17 doubleToString should" - {
    "return Nil for Nil" in {
      assert(Nil === List.doubleToString(Nil))
    }

    "return List(\"1.0\",\"2.0\") for List(1.0,2.0)" in {
      assert(List("1.0", "2.0") === List.doubleToString(List(1.0,2.0)))
    }
  }

  "3.18 map should" - {
    "leave nil regardles of transformation" in {
      assert(Nil === List.map(Nil: List[Int])(_.toString))
    }

    "preserve order and tranform arguments" in {
      assert(List("1","2","3") === List.map(List(1,2,3))(_.toString))
    }
  }

  "3.19 filter should" - {
    "leave nil regardles of predicate" in {
      assert(Nil === List.filter(Nil: List[Int])(_ % 2 == 1))
    }

    "preserve order and leave only elements passing filter" in {
      assert(List(1,3) === List.filter(List(1,2,3))(_%2==1))
    }
  }

  "3.20 flatMap should" - {
    "leave nil regardles of transformation" in {
      assert(Nil === List.flatMap(Nil: List[Int])(i => List(i.toString)))
    }

    "convertt to list of ints to list of strings" in {
      assert(List("1","2","3") === List.flatMap(List(1,2,3))(i => List(i.toString)))
    }

    "convertt to list of lists of ints to list of strings" in {
      assert(List("1","2","3") === List.flatMap(List(List(1,2),List(3)))(i => List.map(i)(_.toString)))
    }
  }

  "3.21 filterAsFM should" - {
    "leave nil regardles of predicate" in {
      assert(Nil === List.filterAsFM(Nil: List[Int])(_ % 2 == 1))
    }

    "preserve order and leave only elements passing filter" in {
      assert(List(1,3) === List.filterAsFM(List(1,2,3))(_%2==1))
    }
  }

  "3.22 sumLists should" - {
    "add corresponding elements" in {
      val result = List.sumLists(List(1, 2, 3), List(2, 3, 4))
      assert(List(3,5,7) === result)
    }

    "handle Nil, Nil case" in {
      val result = List.sumLists(Nil, Nil)
      assert(Nil === result)
    }

    "handle Nil in l1" in {
      val result = List.sumLists(Nil, List(2, 3, 4))
      assert(List(2,3,4) === result)
    }

    "handle Nil in l2" in {
      val result = List.sumLists(List(2, 3, 4), Nil)
      assert(List(2,3,4) === result)
    }
  }

  "3.23 zipWith should" - {
    "add corresponding elements" in {
      val result = List.zipWith(List(1, 2, 3), List(2, 3, 4), Some(0), Some(0))(_+_)
      assert(List(3,5,7) === result)
    }

    "handle Nil, Nil case" in {
      val result = List.zipWith(Nil:List[Int], Nil:List[Int], Some(0), Some(0))(_+_)
      assert(Nil === result)
    }

    "handle Nil in l1" in {
      val result = List.zipWith(Nil:List[Int], List(2, 3, 4), Some(0), Some(0))(_+_)
      assert(List(2,3,4) === result)
    }

    "handle Nil in l2" in {
      val result = List.zipWith(List(2, 3, 4), Nil:List[Int], Some(0), Some(0))(_+_)
      assert(List(2,3,4) === result)
    }
  }

  "3.24 hasSubsequence should" - {
    "find simple pattern - one element" in {
      assert(true === List.hasSubsequence(List(1,2,3), List(1)))
    }

    "find simple pattern - two elements" in {
      assert(true === List.hasSubsequence(List(1,2,3), List(1,2)))
    }

    "find complex pattern" in {
      assert(true === List.hasSubsequence(List(1,2,1,2,1,2,3), List(1,2,1,2,3)))
    }

    "return false for Nil list" in {
      assert(false === List.hasSubsequence(Nil, List(1,2,1,2,3)))
    }

    "return true for Nil pattern" in {
      assert(true === List.hasSubsequence(List(1,2,1,2,3), Nil))
    }
  }
}
