package pers.fpinscala.chapter8

import org.scalamock.scalatest.MockFactory
import org.scalatest.FreeSpec
import pers.fpinscala.chapter6.RNG

/**
  * Created by bajorl on 08/07/2016.
  */
class GenTest extends FreeSpec with MockFactory {
  "ex 8.5 should" - {
    "should return some random nums" in {
      val mockedRng1 = mock[RNG]
      val mockedRng2 = mock[RNG]
      val mockedRng3 = mock[RNG]

      (mockedRng1.nextInt _).expects().returns((1, mockedRng2))
      (mockedRng2.nextInt _).expects().returns((2, mockedRng3))

      val listGen = Gen.listOfN(2, Gen.choose(0, 10))

      val result = listGen.sample.run(mockedRng1)

      println(result)
      assert((List(1, 2), mockedRng3) === result)
    }
  }
}
