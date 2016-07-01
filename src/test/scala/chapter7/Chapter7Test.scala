package pers.fpinscala.chapter7

import java.util.concurrent.{ExecutorService, Executors}

import org.scalatest.FreeSpec

/**
  * Created by bajorl on 27/06/2016.
  */
class Chapter7Test extends FreeSpec {
  val es = Executors.newFixedThreadPool(10)

  "sum should" - {
    "return 1 for List(1)" in {
      assert(1 === Chapter7.sum(es)(List(1)))
    }

    "return 3 for List(1, 2)" in {
      assert(3 === Chapter7.sum(es)(List(1, 2)))
    }

    "return 0 for empty list" in {
      assert(0 === Chapter7.sum(es)(Nil))
    }
  }

  "max should" - {
    "return 1 for List(1)" in {
      assert(1 === Chapter7.max(es)(List(1)))
    }

    "return 2 for List(1, 2)" in {
      assert(2 === Chapter7.max(es)(List(1, 2)))
    }

    "return Int.MinValue for empty list" in {
      assert(Int.MinValue === Chapter7.max(es)(Nil))
    }
  }

  "count words should" - {
    "return 1 for List(\"def\")" in {
      assert(1 === Chapter7.countWords(es)(List("def")))
    }

    "return 2 for List(\"abc def\")" in {
      assert(2 === Chapter7.countWords(es)(List("abc def")))
    }

    "return 5 for List(\"abc def\", \"a b\", \"a\")" in {
      assert(5 === Chapter7.countWords(es)(List("abc def", "a b", "a")))
    }

    "return 0 for empty list" in {
      assert(0 === Chapter7.countWords(es)(Nil))
    }
  }

  "7.11 choice should" - {
    "return t for Par(true)" in {
      assert(Left(1) === Par.run(es)(Par.choice(Par.unit(true))(Par.unit(1), Par.unit(2))))
    }

    "return f for Par(false)" in {
      assert(Left(2) === Par.run(es)(Par.choice(Par.unit(false))(Par.unit(1), Par.unit(2))))
    }
  }
}
