package chapter5

import org.scalatest.FreeSpec

import scala.collection.immutable
import scala.collection.immutable.Nil

/**
  * Created by lbajor on 2016-06-08.
  */
class StreamShould extends FreeSpec {

  "ex 5.1 toList should" - {
    "return Nil for empty" in {
      assert(Nil === Empty.toList)
    }

    "return List(1) for Stream(1, Empty)" in {
      assert(List(1) === Stream.cons(1, Empty).toList)
    }

    "return List(1,2) for Cons(1, Cons(2, Empty))" in {
      assert(List(1,2) === Stream(1,2).toList)
    }
  }

  "ex 5.2 take should" - {
    "return empty for empty" in {
      assert(Empty === Empty.take(3))
    }

    "return empty for n < 0" in {
      assert(Empty === Stream(1).take(-1))
    }

    "return empty for n == 0" in {
      assert(Empty === Stream(1).take(0))
    }

    "return Cons(1, Empty) for Cons(1, Empty) and n == 1" in {
      assert(List(1) === Stream(1).take(1).toList)
    }

    "evaluate none elements when taking 2 els from 3 el Stream" in {
      var evaluated1 = false
      var evaluated2 = false
      var evaluated3 = false

      Stream.cons(
        { evaluated1=true; 1},
        Stream.cons({evaluated2=true; 2},
          Stream.cons({evaluated3 = true; 3}, Empty)
        )
      ).take(2)

      assert(false === evaluated1)
      assert(false === evaluated2)
      assert(false === evaluated3)
    }
  }

  "ex 5.2 drop should" - {
    "return empty for empty.drop(3)" in {
      assert(Empty === Empty.drop(3))
    }

    "return empty for empty.drop(-1)" in {
      assert(Empty === Empty.drop(-1))
    }

    "return same for n < 0" in {
      assert(List(1) === Stream(1).drop(-1).toList)
    }

    "return same for n == 0" in {
      assert(List(1) === Stream(1).drop(0).toList)
    }

    "return Empty for Stream(1,2,3) and n = 4" in {
      assert(Empty === Stream(1, 2, 3).drop(4))
    }

    "return Stream(3) for Stream(1,2,3).drop(2)" in {
      assert(List(3) === Stream(1, 2, 3).drop(2).toList)
    }

    "evaluate none elements when droping 1 el from 3 el Stream" in {
      var evaluated1 = false
      var evaluated2 = false
      var evaluated3 = false

      Stream.cons(
        { evaluated1=true; 1},
        Stream.cons({evaluated2=true; 2},
          Stream.cons({evaluated3 = true; 3}, Empty))
      ).drop(1)

      assert(false === evaluated1)
      assert(false === evaluated2)
      assert(false === evaluated3)
    }
  }

  "ex 5.3 takeWhile should" - {
    "return Stream(1,2) for Stream(1,2,3,4).takeWhile(_<=2)" in {
      assert(List(1,2) === Stream(1,2,3,4).takeWhile(_<=2).toList)
    }

    "return Stream(1,2) for Stream(1,2).takeWhile(e => true)" in {
      assert(List(1,2) === Stream(1,2).takeWhile(e => true).toList)
    }

    "return empty for Stream(1,2).takeWhile(e => false)" in {
      assert(Empty === Stream(1,2).takeWhile(e => false))
    }

    "return Empty for Empty.takeWhile(e => true)" in {
      assert(Empty === Empty.takeWhile(e => true))
    }
  }

  "ex 5.4 forAll should" - {
    "return true for Stream(1,2,3,4).forAll(_<=5)" in {
      assert(true === Stream(1,2,3,4).forAll(_<=5))
    }

    "return false for Stream(1,2).forAll(_%2==1)" in {
      assert(false === Stream(1,2).forAll(_%2==1))
    }

    "return return true for empty" in {
      assert(true === Empty.forAll(e => false))
    }

    "short circut on first false" in {
      var evaluated1 = false
      var evaluated2 = false
      var evaluated3 = false

      Stream.cons(
        { evaluated1=true; 1},
        Stream.cons({evaluated2=true; 2},
          Stream.cons({evaluated3 = true; 3}, Empty))
      ).forAll(_ < 2)

      assert(evaluated1 === true)
      assert(evaluated2 === true)
      assert(evaluated3 === false)
    }
  }

  "ex 5.5 takeWhileFoldRight should" - {
    "return Stream(1,2) for Stream(1,2,3,4).takeWhileFoldRight(_<=2)" in {
      assert(List(1,2) === Stream(1,2,3,4).takeWhileFoldRight(_<=2).toList)
    }

    "return Stream(1,2) for Stream(1,2).takeWhileFoldRight(e => true)" in {
      assert(List(1,2) === Stream(1,2).takeWhileFoldRight(e => true).toList)
    }

    "return empty for Stream(1,2).takeWhileFoldRight(e => false)" in {
      assert(Empty === Stream(1,2).takeWhileFoldRight(e => false))
    }

    "return Empty for Empty.takeWhileFoldRight(e => true)" in {
      assert(Empty === Empty.takeWhileFoldRight(e => true))
    }
  }

  "ex 5.6 headOptionFR should" - {
    "return Some(1) for Stream(1,2).headOptionFR" in {
      assert(Some(1) === Stream(1,2).headOptionFR)
    }

    "return None for Empty.headOptionFR" in {
      assert(None === Empty.headOptionFR)
    }

    "not evaluate second el" in {
      var evaluated1 = false
      var evaluated2 = false

      Stream.cons(
        { evaluated1=true; 1},
        Stream.cons({evaluated2=true; 2}, Empty)
      ).headOptionFR

      assert(evaluated1 === true)
      assert(evaluated2 === false)
    }
  }

  "ex 5.7 map should" - {
    "return Stream(\"1\") for Stream(1).map(_.toString)" in {
      assert(List("1") === Stream(1).map(_.toString).toList)
    }

    "return Empty for Empty.map(e=>1)" in {
      assert(Empty === Empty.map(e=>1))
    }

    "not evaluate second el" in {
      var evaluated1 = false
      var evaluated2 = false

      Stream.cons(
        { evaluated1=true; 1},
        Stream.cons({evaluated2=true; 2}, Empty)
      ).map(_.toString)

      assert(evaluated1 === true)
      assert(evaluated2 === false)
    }
  }

  "ex 5.7 filter should" - {
    "return Stream(1) for Stream(1,2).map(_%2==1)" in {
      assert(List(1) === Stream(1,2).filter(_%2==1).toList)
    }

    "return Empty for Empty.filter(e=>true)" in {
      assert(Empty === Empty.filter(e=>true))
    }

    "not evaluate second el" in {
      var evaluated1 = false
      var evaluated2 = false

      Stream.cons(
        { evaluated1=true; 1},
        Stream.cons({evaluated2=true; 2}, Empty)
      ).filter(e => true)

      assert(evaluated1 === true)
      assert(evaluated2 === false)
    }
  }

  "ex 5.7 append should" - {
    "return Stream(1, 2) for Stream(1).append({2})" in {
      assert(List(1, 2) === Stream(1).append({2}).toList)
    }

    "return Stream(1) for Empty.append(1)" in {
      assert(List(1) === Empty.append(1).toList)
    }
  }

  "ex 5.7 flatMap should" - {
    "return Stream(1, 2) for Stream(1,2).flatMap(e => Stream.cons(e, Empty))" in {
      assert(List(1, 2) === Stream(1,2).flatMap(e => Stream.cons(e, Empty)).toList)
    }

    "return Empty for Empty.flatMap(e => Stream.cons(e, Empty))" in {
      assert(Empty === Empty.flatMap(e => Stream.cons(e, Empty)))
    }
  }
}
