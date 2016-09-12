package pers.fpinscala.chapter4

import org.scalatest.FreeSpec

/**
  * Created by bajorl on 02/06/2016.
  */
class Chapter4Should extends FreeSpec {

  "exercise 4.2 variance should" - {
    "return None for empty Seq" in {
      assert(None === Chapter4.variance(Nil: List[Double]))
    }

    "return 0.5 for Seq(1,2)" in {
      assert(Some(0.25) === Chapter4.variance(Seq(1.0, 2.0)))
    }
  }

  "exercise 4.3 map2 should" - {
    "return Some(3) for map2(Some(1),Some(2))(_+_)" in {
      assert(Some(3) === Chapter4.map2(Some(1), Some(2))(_+_))
    }

    "return None for map2(None,_)(_+_)" in {
      assert(None === Chapter4.map2(None:Option[Int], Some(2))(_+_))
    }

    "return None for map2(_,None)(_+_)" in {
      assert(None === Chapter4.map2(Some(1), None:Option[Int])(_+_))
    }
  }

  "exercise 4.4 sequence should" - {
    "return Some(List(1)] for List(Some(1))" in {
      assert(Some(List(1)) === Chapter4.sequence(List(Some(1))))
    }

    "return Some(List(1, 2)] for List(Some(1), Some(2))" in {
      assert(Some(List(1, 2)) === Chapter4.sequence(List(Some(1), Some(2))))
    }

    "return None for List(Some(1), None)" in {
      assert(None === Chapter4.sequence(List(Some(1), None)))
    }

    "return None for List(None, Some(1))" in {
      assert(None === Chapter4.sequence(List(None, Some(1))))
    }
  }

  "exercise 4.5 traverse should" - {
    "return Some(List(1)] for List(1) and func returning Some(x) for odd and None for even" in {
      assert(Some(List(1)) === Chapter4.traverse(List(1))(i => if (i % 2 == 1) Some(i) else None))
    }

    "return None for List(1,2) and func returning Some(x) for odd and None for even" in {
      assert(None === Chapter4.traverse(List(1,2))(i => if (i % 2 == 1) Some(i) else None))
    }

    "return Nil for None and func returning Some(x) for odd and None for even" in {
      assert(Some(Nil) === Chapter4.traverse(Nil:List[Int])(i => if (i % 2 == 1) Some(i) else None))
    }
  }

  "exercise 4.5 sequenceAsTraverse should" - {
    "return Some(List(1)] for List(Some(1))" in {
      assert(Some(List(1)) === Chapter4.sequenceAsTraverse(List(Some(1))))
    }

    "return Some(List(1, 2)] for List(Some(1), Some(2))" in {
      assert(Some(List(1, 2)) === Chapter4.sequenceAsTraverse(List(Some(1), Some(2))))
    }

    "return None for List(Some(1), None)" in {
      assert(None === Chapter4.sequenceAsTraverse(List(Some(1), None)))
    }

    "return None for List(None, Some(1))" in {
      assert(None === Chapter4.sequenceAsTraverse(List(None, Some(1))))
    }
  }
}
