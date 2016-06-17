package pers.fpinscala.chapter6

import org.scalatest.FreeSpec

/**
  * Created by bajorl on 16/06/2016.
  */
class MachineShould extends FreeSpec {

  "Machine.simulate should " - {
    "unlock locked machine when coin is inserted if candies are in" in {
      val m = Machine(true, 1, 1)

      assert(((1, 2), Machine(false, 1, 2))  === Machine.simulate(List(Coin)).run(m))
    }

    "ignore coin on unlocked machine" in {
      val m = Machine(false, 1, 1)

      assert(((1, 1), Machine(false, 1, 1))  === Machine.simulate(List(Coin)).run(m))
    }

    "ignore coin on machine without candies" in {
      val m = Machine(true, 0, 1)

      assert(((0, 1), Machine(true, 0, 1))  === Machine.simulate(List(Coin)).run(m))
    }

    "give candy when turning unlocked machine" in {
      val m = Machine(false, 1, 1)

      assert(((0, 1), Machine(true, 0, 1))  === Machine.simulate(List(Turn)).run(m))
    }

    "ignore turn on locked machine" in {
      val m = Machine(true, 1, 1)

      assert(((1, 1), Machine(true, 1, 1))  === Machine.simulate(List(Turn)).run(m))
    }
  }

  "Machine.simulateState1 should " - {
    "unlock locked machine when coin is inserted if candies are in" in {
      val m = Machine(true, 1, 1)

      assert(((1, 2), Machine(false, 1, 2))  === Machine.simulateState1(List(Coin)).run(m))
    }

    "ignore coin on unlocked machine" in {
      val m = Machine(false, 1, 1)

      assert(((1, 1), Machine(false, 1, 1))  === Machine.simulateState1(List(Coin)).run(m))
    }

    "ignore coin on machine without candies" in {
      val m = Machine(true, 0, 1)

      assert(((0, 1), Machine(true, 0, 1))  === Machine.simulateState1(List(Coin)).run(m))
    }

    "give candy when turning unlocked machine" in {
      val m = Machine(false, 1, 1)

      assert(((0, 1), Machine(true, 0, 1))  === Machine.simulateState1(List(Turn)).run(m))
    }

    "ignore turn on locked machine" in {
      val m = Machine(true, 1, 1)

      assert(((1, 1), Machine(true, 1, 1))  === Machine.simulateState1(List(Turn)).run(m))
    }
  }

  "Machine.simulateState2 should " - {
    "unlock locked machine when coin is inserted if candies are in" in {
      val m = Machine(true, 1, 1)

      assert(((1, 2), Machine(false, 1, 2))  === Machine.simulateState2(List(Coin)).run(m))
    }

    "ignore coin on unlocked machine" in {
      val m = Machine(false, 1, 1)

      assert(((1, 1), Machine(false, 1, 1))  === Machine.simulateState2(List(Coin)).run(m))
    }

    "ignore coin on machine without candies" in {
      val m = Machine(true, 0, 1)

      assert(((0, 1), Machine(true, 0, 1))  === Machine.simulateState2(List(Coin)).run(m))
    }

    "give candy when turning unlocked machine" in {
      val m = Machine(false, 1, 1)

      assert(((0, 1), Machine(true, 0, 1))  === Machine.simulateState2(List(Turn)).run(m))
    }

    "ignore turn on locked machine" in {
      val m = Machine(true, 1, 1)

      assert(((1, 1), Machine(true, 1, 1))  === Machine.simulateState2(List(Turn)).run(m))
    }
  }
}
