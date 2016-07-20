package pers.fpinscala.chapter8

import pers.fpinscala.chapter6.{State, RNG}

/**
  * Created by bajorl on 07/07/2016.
  */

case class Gen[A](sample: State[RNG, A]) {
  //8.6
  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    Gen[B](
      State[RNG, B](
      (r: RNG) => {
      val (el, nRng) = sample.run(r)
      f(el).sample.run(nRng)
    }))
  }

  def listOfN(size: Int): Gen[List[A]] = Gen.listOfN(size, this)

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(lSize => Gen.listOfN(lSize, this))

  //8.10
  def unsized: SGen[A] = SGen[A]((i: Int) => this)

  def map[B](f: A => B): Gen[B] = flatMap(a => Gen.unit(f(a)))

  def map2[B, C](b: Gen[B])(f: (A, B) => C): Gen[C] =
    flatMap(a => b.map(b => f(a, b)))

  def **[B](g: Gen[B]): Gen[(A,B)] =
    (this map2 g)((_,_))
}

object Gen {
  //8.4
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen[Int](
      State(
        (r: RNG) => {
          val (i, nR) = RNG.nonNegativeInt(r)
          (start + (i % (stopExclusive - start)), nR)
        }))
  }

  //8.5
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = {
    Gen[Boolean](
      State(
        (r: RNG) => {
          val (i, nR) = r.nextInt
          (i % 2 == 0, nR)
        }
      )
    )
  }

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    if (n <= 0) unit(Nil:List[A])
    else Gen[List[A]](
      g.sample.flatMap(
        h => listOfN(n-1, g).sample.map(t => h::t)
      )
    )
  }

  //8.7
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap(if (_) g1 else g2)

  //8.8
  def double(min: Double, maxExclusive: Double): Gen[Double] = {
    Gen[Double](
      State(
        (r: RNG) => {
          val (d, nRng) = RNG.double(r)
          (min + (d / (maxExclusive-min)), nRng)
        }
      )
    )
  }

  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
    double(0, g1._2 + g2._2).flatMap(w => if (g1._2 >= w) g1._1 else g2._1)
  }
}
