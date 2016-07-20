package pers.fpinscala.chapter8

import java.util.concurrent.{Executors, ExecutorService}

import org.scalatest.FreeSpec
import pers.fpinscala.chapter7.Par
import pers.fpinscala.chapter7.Par._

/**
  * Created by bajorl on 13/07/2016.
  */

class Chapter8Should extends FreeSpec {

  "chapter8 should" - {
    //8.13
    "succeed maxProp1" in {
      assert(true === PropMaxSize.myRun(
        Chapter8.maxProp1
      ))
    }

    "succeed sorterProp" in {
      assert(true === PropMaxSize.myRun(
        Chapter8.sortedProp
      ))
    }

    val ES: ExecutorService = Executors.newCachedThreadPool

    val S = Gen.weighted(
      (Gen.choose(1, 4).map(Executors.newFixedThreadPool), .75),
      (Gen.unit(Executors.newCachedThreadPool), .25)
    )

    def checkPar = Chapter8.checkPar(ES) _
    "test par addition" in {
      checkPar {
        Chapter8.equal(
          Par.map(Par.unit(1))(_ + 1),
          Par.unit(2)
        )
      }
    }

    def forAllPar[A] = Chapter8.forAllPar[A](S) _
    "test par equality" in {
      forAllPar(Chapter8.pint) {
        n => Chapter8.equal(
          Par.map(n)(y => y),
          n
        )
      }
    }

    //8.17
    "test fork(x) == x" in {
      forAllPar(Chapter8.pint2) {
        n => Chapter8.equal(
          Par.fork(n),
          n
        )
      }
    }

    //8.18
    "test takeWhile by dropWhile l.takeWhile(f) == l.dropWhile(!f)" in {
      PropMaxSize.myRun(
        Chapter8.takeWhileProp(_ < 4)
      )
    }
  }
}
