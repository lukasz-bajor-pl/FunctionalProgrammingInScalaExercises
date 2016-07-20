package pers.fpinscala.chapter8

import pers.fpinscala.chapter6.{RNG, State}

/**
  * Created by bajorl on 11/07/2016.
  */
case class SGen[A](forSize: Int => Gen[A]) {
  //8.11
  def flatMap[B](f: A => Gen[B]): SGen[B] = {
    SGen[B](
      (i: Int) => forSize(i).flatMap(f)
    )
  }
}

object SGen {
  //8.11
  def unit[A](a: => A): SGen[A] = Gen(State.unit(a)).unsized

  def boolean: SGen[Boolean] = Gen.boolean.unsized

  def union[A](g1: Gen[A], g2: Gen[A]): SGen[A] =
    boolean.flatMap(if (_) g1 else g2)

  //8.12
  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen[List[A]]((size: Int) => Gen.listOfN(size, g))

  //8.13
  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen(n => g.listOfN(n max 1))
}
