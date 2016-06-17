package pers.fpinscala.chapter6

import scala.Stream._

/**
  * Created by bajorl on 16/06/2016.
  */
trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECEE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {
  def int(rng: RNG): (Int, RNG) = {
    rng.nextInt
  }

  //6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, newRng) = rng.nextInt

    (
      if (n == Int.MinValue) Int.MaxValue
      else if (n < 0) -1 * n
      else n,

      newRng
      )
  }

  //6.2
  def double(rng: RNG): (Double, RNG) = {
    val (i, newRng) = nonNegativeInt(rng)

    (i.min(Int.MaxValue - 1).toDouble / Int.MaxValue, newRng)
  }

  //6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, nR) = nonNegativeInt(rng)
    val (d, rR) = double(nR)

    ((i, d), rR)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), rR) = intDouble(rng)
    ((d, i), rR)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng);
    val (d2, r2) = double(r1);
    val (d3, r3) = double(r2);

    ((d1, d2, d3), r3)
  }

  //6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def intStream(rng: RNG): Stream[(Int, RNG)] = {
      val (n, r) = rng.nextInt
      cons((n, r), {
        intStream(r)
      })
    }

    val numbersAndRandoms = (1 to count).zip(intStream(rng))
    val listOfNums = numbersAndRandoms.map {
      {
        case (idx, (num, rng)) => num
      }
    }.toList
    val rRng = numbersAndRandoms.last._2._2

    (listOfNums, rRng)
  }

  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  //6.5
  def doubleMap: Rand[Double] = map(nonNegativeInt)(
    i => i.min(Int.MaxValue - 1).toDouble / Int.MaxValue
  )

  //6.6
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] =
    both(nonNegativeInt, double)

  val randDoubleInt: Rand[(Double, Int)] =
    both(double, nonNegativeInt)

  //6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    rng => fs.foldRight({
      (Nil: List[A], rng)
    })({ case (f, (pL, pRng)) =>
      map(f)(pL ::: List(_))(pRng)
    })
  }

  def intsAsSeq(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  //6.8
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }
  }

  def mapAsFM[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(e => rng => (f(e), rng))

  def map2AsFM[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => rng => {
      val (b, rng2) = rb(rng)
      (f(a, b), rng2)
    })
}
