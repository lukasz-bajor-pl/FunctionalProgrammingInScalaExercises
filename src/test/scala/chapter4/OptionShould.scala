package chapter4

import org.scalatest.FreeSpec

/**
  * Created by bajorl on 02/06/2016.
  */
class OptionShould extends FreeSpec {

  //exercise 4.1
  "map should" - {
    "return None for None" in {
      assert(None === None.map(x => "Abc"))
    }

    "return new Some(\"1\") for Some(int)" in {
      assert(Some("1") === Some(1).map(_.toString))
    }
  }

  "flatMap should" - {
    "return None for None" in {
      assert(None === None.flatMap(x => Some("Abc")))
    }

    "return None for None and func returning None" in {
      assert(None === Some(1).flatMap(x => None))
    }

    "return None for Some(x) and func returning Some(y)" in {
      assert(Some("a") === Some(1).flatMap(x => Some("a")))
    }
  }

  "getOrElse should" - {
    "return Else for None" in {
      assert("Abc" === None.getOrElse("Abc"))
    }

    "return x for Some(x)" in {
      assert("a" === Some("a").getOrElse("Abc"))
    }
  }

  "orElse should" - {
    "return Else for None" in {
      assert(Some(123) === None.orElse(Some(123)))
    }

    "return Some(x) for Some(x)" in {
      assert(Some(1) === Some(1).orElse(Some(2)))
    }
  }

  "filter should" - {
    "return None for None" in {
      assert(None === (None:Option[Int]).filter(e => e % 2 == 1))
    }

    "return None for Some(x) where x is not matching filter" in {
      assert(None === (Some(2)).filter(e => e % 2 == 1))
    }

    "return Some(x) for Some(x) where x is matching filter" in {
      assert(Some(1) === (Some(1)).filter(e => e % 2 == 1))
    }
  }
}
