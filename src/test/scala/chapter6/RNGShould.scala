package pers.fpinscala.chapter6

import org.scalamock.scalatest.MockFactory
import org.scalatest.FreeSpec

/**
  * Created by bajorl on 16/06/2016.
  */
class RNGShould extends FreeSpec with MockFactory {
  val mockedRng = mock[RNG]
  val newMockedRng = mock[RNG]
  val newMockedRng2 = mock[RNG]

  "ex 6.1 nonNegativeInt should" - {

    "return rng.nextInt if rng returned > 0" in {
      val returnedInt = 1

      (mockedRng.nextInt _) expects() returning((returnedInt, newMockedRng))

      assert((returnedInt,newMockedRng) === RNG.nonNegativeInt(mockedRng))
    }

    "return rng.nextInt if rng returned 0" in {
      val returnedInt = 0

      (mockedRng.nextInt _) expects() returning((returnedInt, newMockedRng))

      assert((returnedInt,newMockedRng) === RNG.nonNegativeInt(mockedRng))
    }

    "return rng.nextInt if rng returned Int.MaxValue" in {
      val returnedInt = Int.MaxValue

      (mockedRng.nextInt _) expects() returning((returnedInt, newMockedRng))

      assert((returnedInt,newMockedRng) === RNG.nonNegativeInt(mockedRng))
    }

    "return -1*rng.nextInt if rng returned < 0" in {
      val returnedInt = -1

      (mockedRng.nextInt _) expects() returning((returnedInt, newMockedRng))

      assert((returnedInt * -1,newMockedRng) === RNG.nonNegativeInt(mockedRng))
    }

    "return Int.MaxValue if rng returned Int.MinValue" in {
      val returnedInt = Int.MinValue

      (mockedRng.nextInt _) expects() returning((returnedInt, newMockedRng))

      assert((Int.MaxValue,newMockedRng) === RNG.nonNegativeInt(mockedRng))
    }
  }

  "ex 6.2 double should" - {
    "return rng.nextInt / Int.MaxValue if rng returned > 0" in {
      val returnedInt = 1

      (mockedRng.nextInt _) expects() returning((returnedInt, newMockedRng))

      assert((1.0d/Int.MaxValue,newMockedRng) === RNG.double(mockedRng))
    }

    "return -rng.nextInt / Int.MaxValue if rng returned < 0" in {
      val returnedInt = -1

      (mockedRng.nextInt _) expects() returning((returnedInt, newMockedRng))

      assert((1.0d/Int.MaxValue,newMockedRng) === RNG.double(mockedRng))
    }

    "return < 1 if rng returned Int.MaxValue" in {
      val returnedInt = Int.MaxValue

      (mockedRng.nextInt _) expects() returning((returnedInt, newMockedRng))
      val (r, _) = RNG.double(mockedRng)

      assert(r < 1.0)
    }

    "return < 1 if rng returned Int.MinValue" in {
      val returnedInt = Int.MinValue

      (mockedRng.nextInt _) expects() returning((returnedInt, newMockedRng))
      val (r, _) = RNG.double(mockedRng)

      assert(r < 1.0)
    }
  }

  "ex 6.3 intDouble should" - {
    "return values returned by rng.nextInt" in {
      val returnedInt1 = 10
      val returnedInt2 = -5

      (mockedRng.nextInt _) expects() returning((returnedInt1, newMockedRng))
      (newMockedRng.nextInt _) expects() returning((returnedInt2, newMockedRng2))

      assert(((returnedInt1, 5.0d/Int.MaxValue), newMockedRng2) === RNG.intDouble(mockedRng))
    }
  }

  "ex 6.3 doubleInt should" - {
    "return values in reverse of intDouble" in {
      val returnedInt1 = 10
      val returnedInt2 = -5

      (mockedRng.nextInt _) expects() returning((returnedInt1, newMockedRng))
      (newMockedRng.nextInt _) expects() returning((returnedInt2, newMockedRng2))

      assert(((5.0d/Int.MaxValue, returnedInt1), newMockedRng2) === RNG.doubleInt(mockedRng))
    }
  }

  "ex 6.3 double3 should" - {
    "return 3 values of double" in {
      val returnedInt1 = 10
      val returnedInt2 = -5
      val returnedInt3 = 0

      val newMockedRng3 = mock[RNG]

      (mockedRng.nextInt _) expects() returning((returnedInt1, newMockedRng))
      (newMockedRng.nextInt _) expects() returning((returnedInt2, newMockedRng2))
      (newMockedRng2.nextInt _) expects() returning((returnedInt3, newMockedRng3))

      assert(((10.0d/Int.MaxValue, 5.0d/Int.MaxValue, 0), newMockedRng3) === RNG.double3(mockedRng))
    }
  }

  "ex 6.4 ints should" - {
    "return 3 values of nextInt" in {
      val returnedInt1 = 10
      val returnedInt2 = -5
      val returnedInt3 = 0

      val newMockedRng3 = mock[RNG]

      (mockedRng.nextInt _) expects() returning((returnedInt1, newMockedRng))
      (newMockedRng.nextInt _) expects() returning((returnedInt2, newMockedRng2))
      (newMockedRng2.nextInt _) expects() returning((returnedInt3, newMockedRng3))

      assert((List(returnedInt1, returnedInt2, returnedInt3), newMockedRng3) === RNG.ints(3)(mockedRng))
    }
  }

  "ex 6.5 doubleMap should work as double" - {
    "return rng.nextInt / Int.MaxValue if rng returned > 0" in {
      val returnedInt = 1

      (mockedRng.nextInt _) expects() returning((returnedInt, newMockedRng))

      assert((1.0d/Int.MaxValue,newMockedRng) === RNG.doubleMap(mockedRng))
    }

    "return -rng.nextInt / Int.MaxValue if rng returned < 0" in {
      val returnedInt = -1

      (mockedRng.nextInt _) expects() returning((returnedInt, newMockedRng))

      assert((1.0d/Int.MaxValue,newMockedRng) === RNG.doubleMap(mockedRng))
    }

    "return < 1 if rng returned Int.MaxValue" in {
      val returnedInt = Int.MaxValue

      (mockedRng.nextInt _) expects() returning((returnedInt, newMockedRng))
      val (r, _) = RNG.doubleMap(mockedRng)

      assert(r < 1.0)
    }

    "return < 1 if rng returned Int.MinValue" in {
      val returnedInt = Int.MinValue

      (mockedRng.nextInt _) expects() returning((returnedInt, newMockedRng))
      val (r, _) = RNG.doubleMap(mockedRng)

      assert(r < 1.0)
    }
  }

  "ex 6.7 sequence/intsAsSeq should work as double" - {
    "return 3 values of nextInt" in {
      val returnedInt1 = 10
      val returnedInt2 = -5
      val returnedInt3 = 0

      val newMockedRng3 = mock[RNG]

      (mockedRng.nextInt _) expects() atLeastOnce() returning((returnedInt1, newMockedRng))
      (newMockedRng.nextInt _) expects() atLeastOnce() returning((returnedInt2, newMockedRng2))
      (newMockedRng2.nextInt _) expects() atLeastOnce() returning((returnedInt3, newMockedRng3))

      val r1 = RNG.intsAsSeq(3)(mockedRng)

      println(r1)

      assert((List(returnedInt1, returnedInt2, returnedInt3), newMockedRng3) === r1)
    }
  }
}
