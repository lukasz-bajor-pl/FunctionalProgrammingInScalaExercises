package pers.fpinscala.chapter6

/**
  * Created by bajorl on 16/06/2016.
  */

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine {
  def simulate(inputs: List[Input]): State[Machine, (Int, Int)] = {
    State(
      (machine: Machine) => {
        val finalMachine = inputs.foldRight(machine)(
          { case (in, m) => invokeInput(in)(m)
          })

        ((finalMachine.candies, finalMachine.coins), finalMachine)
      }
    )
  }

  //do alternative impl if possible using state modify get set

  private def invokeInput(i: Input)(m: Machine): Machine = {
    (m, i) match {
      case (m@Machine(_, 0, _), _) => m
      case (Machine(true, ca, co), Coin) => Machine(false, ca, co + 1)
      case (m@Machine(false, _, _), Coin) => m
      case (m@Machine(true, _, _), Turn) => m
      case (Machine(false, ca, co), Turn) => Machine(true, ca - 1, co)
    }
  }

  def simulateState1(inputs: List[Input]): State[Machine, (Int, Int)] = {
    val finalState = inputs.foldRight(State.get[Machine])({
      case (in, s) => s.map(m => invokeInput(in)(m))
    })

    finalState.flatMap(m => State(s => ((m.candies, m.coins), m)))
  }

  def simulateState2(inputs: List[Input]): State[Machine, (Int, Int)] = {
    val subResult =
      inputs.map(
        in => State.modify((m: Machine) => invokeInput(in)(m))
      ).foldRight(
      State.unit[Unit, Machine](())
    )({ case (mod, res) => res.map2(mod)(
      { case (mach, unit) => unit}
    )})

    subResult.flatMap(_ => State(s => ((s.candies, s.coins), s)))
  }
}
