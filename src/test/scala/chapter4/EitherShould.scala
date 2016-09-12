package pers.fpinscala.chapter4

import org.scalatest.FreeSpec

/**
  * Created by bajorl on 02/06/2016.
  */
class EitherShould extends FreeSpec {

  "4.6 map should" - {
    "return Left(e) for Left(e)" in {
      assert(Left("Abc") === (Left("Abc"):Either[String, String]).map(x => x.toUpperCase))
    }

    "return Right(\"A\") for Right(\"a\")" in {
      assert(Right("A") === Right("a").map(x => x.toUpperCase))
    }
  }

  "4.6 flatMap should" - {
    "return Left(e) for Left(e)" in {
      assert(Left("e") === (Left("e"):Either[String, String]).flatMap(s => Right(s.toUpperCase())))
    }

    "return Left(e) for Right(v) and f(v) = Left(e)" in {
      assert(Left("e") === Right(2).flatMap(x => Left("e")))
    }

    "return Right(\"2\") for Right(2) and f(2) = Right(\"2\")" in {
      assert(Right("2") === Right(2).flatMap(x => Right("2")))
    }
  }

  "4.6 orElse should" - {
    "return b for Left(e)" in {
      assert(Right(1) === (Left("Abc"):Either[Any, Any]).orElse(Right(1)))
    }

    "return either for Right(v)" in {
      assert(Right(2) === Right(2).orElse(Right(1)))
    }
  }

  "4.6 map2 should" - {
    "return Right(\"3\") for Right(1).map2(Right(2))((_+_).toString)" in {
      assert(Right("3") === Right(1).map2(Right(2))((x,y) => (x+y).toString))
    }

    "return Left(\"1\") for Left(\"1\").map2(Right(2))((_+_).toString)" in {
      assert(Left("1") === Left("1").asInstanceOf[Either[String, Int]].map2(Right(2))((x,y) => (x+y).toString))
    }

    "return Left(\"2\") for Right(1).map2(Left(\"2\"))((_+_).toString)" in {
      assert(Left("2") === Right(1).map2(Left("2").asInstanceOf[Either[String, Int]])((x,y) => (x+y).toString))
    }
  }

  "4.7 traverse should" - {
    "return Right(List(1)) for traverse(List(1))(e=>Right(e))" in {
      assert(Right(List(1)) === Either.traverse(List(1))(e => Right(e)))
    }

    "return first left from the list" in {
      assert(Left("Even number 2") === Either.traverse(List(1, 2, 4))(x => if (x % 2 == 1) Right(x.toString) else Left("Even number " + x)))
    }
  }

  "4.8 map2 with error concatenation" - {
    case class Person(name: Name, age: Age)
    sealed case class Name(val value: String)
    sealed case class Age(val value: Int)

    def mkName(name: String): Either[String, Name] =
      if (name == "" || name == null) Left("Name is empty.")
      else Right(new Name(name))

    def mkAge(age: Int): Either[String, Age] =
      if (age < 0) Left("Age is out of range.")
      else Right(new Age(age))

    def mkPerson(name: String, age: Int): Either[List[String], Person] =
      Either.map22(mkName(name), mkAge(age))(Person(_, _))

    "return Right for mkPerson Rick, 22" in {
      assert(Right(Person(Name("Rick"), Age(22))) === mkPerson("Rick", 22))
    }

    "return Left(List(\"Name is empty.\",\"Age is out of range.\")) for mkPerson \"\", -1" in {
      assert(Left(List("Name is empty.","Age is out of range.")) === mkPerson("", -1))
    }

    "return Left(List(\"Name is empty.\")) for mkPerson \"\", 22" in {
      assert(Left(List("Name is empty.")) === mkPerson("", 22))
    }

    "return Left(List(\"Age is out of range.\")) for mkPerson \"Rick\", -1" in {
      assert(Left(List("Age is out of range.")) === mkPerson("Rick", -1))
    }
  }
}
