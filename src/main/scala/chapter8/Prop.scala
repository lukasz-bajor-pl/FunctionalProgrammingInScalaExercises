package pers.fpinscala.chapter8

import pers.fpinscala.chapter5.Stream
import pers.fpinscala.chapter6.{RNG, SimpleRNG}
import pers.fpinscala.chapter8.Prop._

/**
  * Created by bajorl on 07/07/2016.
  */

trait PropTrait {
  def check: Boolean = ???

  //ex 8.3
  def &&(p: PropTrait): PropTrait = {
    lazy val outer = this

    new PropTrait {
      override lazy val check: Boolean = outer.check && p.check
    }
  }
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    def isFalsified = false
  }

  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    def isFalsified = true
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop(
    (n, rng) => randomStream(as)(rng).zipWith(Stream.from(0))((_, _)).take(n).map({
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch {
        case e: Exception => Falsified(buildMsg(a, e), i)
      }
    }).find(_.isFalsified).getOrElse(Passed)
  )

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"gemerated an exception: ${e.getMessage} \n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
}

case class Prop(run: (Prop.TestCases, RNG) => Prop.Result) {
  //8.9
  def &&(p: Prop): Prop = Prop(
    (t: Prop.TestCases, r: RNG) =>
      run(t, r) match {
        case Passed => p.run(t, r)
        case f@Falsified(_, _) => f
      }
  )

  def ||(p: Prop): Prop = Prop(
    (t: Prop.TestCases, r: RNG) =>
      run(t, r) match {
        case Passed => Passed
        case Falsified(failures, successes) => p.run(t, r)
      }
  )
}


object PropMaxSize {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    def isFalsified = false
  }

  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    def isFalsified = true
  }

  case object Proved extends Result {
    def isFalsified = false
  }

  def forAll[A](g: Gen[A])(f: A => Boolean): PropMaxSize = PropMaxSize {
    (_, n, rng) => randomStream(g)(rng).zipWith(Stream.from(0))((_, _)).take(n).map({
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch {
        case e: Exception => Falsified(buildMsg(a, e), i)
      }
    }).find(_.isFalsified).getOrElse(Passed)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"gemerated an exception: ${e.getMessage} \n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
  def forAll[A](g: SGen[A])(f: A => Boolean): PropMaxSize =
    forAll(g.forSize)(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): PropMaxSize = PropMaxSize {
    (max, n, rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[PropMaxSize] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val propsStream = props.map(p => PropMaxSize(
        (max, _, rng) => p.run(max, casesPerSize, rng)
      ))

      val prop = propsStream.toList.reduce(_ && _)

      prop.run(max, n, rng)
    }

  def run(p: PropMaxSize,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = new SimpleRNG(System.currentTimeMillis())): Unit =
    p.run(maxSize, testCases, rng) match {
      case PropMaxSize.Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case PropMaxSize.Passed =>
        println(s"+ OK passed $testCases tests.")
      case PropMaxSize.Proved =>
        println(s"+ OK, proved property.")    }

  def myRun(p: PropMaxSize,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = new SimpleRNG(System.currentTimeMillis())): Boolean =
    p.run(maxSize, testCases, rng) match {
      case PropMaxSize.Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
        false
      case PropMaxSize.Passed =>
        println(s"+ OK passed $testCases tests.")
        true
      case PropMaxSize.Proved =>
        println(s"+ OK, proved property.")
        true
    }

  def check(p: =>Boolean): PropMaxSize = PropMaxSize(
    (_,_,_) => if (p) PropMaxSize.Passed else PropMaxSize.Falsified("()", 0)
  )
}

case class PropMaxSize(run: (PropMaxSize.MaxSize, PropMaxSize.TestCases, RNG) => PropMaxSize.Result) {
  //8.9
  def &&(p: PropMaxSize): PropMaxSize = PropMaxSize(
    (max: PropMaxSize.MaxSize, t: PropMaxSize.TestCases, r: RNG) =>
      run(max, t, r) match {
        case PropMaxSize.Passed => p.run(max, t, r)
        case PropMaxSize.Proved => p.run(max, t, r)
        case f@PropMaxSize.Falsified(_, _) => f
      }
  )

  def ||(p: PropMaxSize): PropMaxSize = PropMaxSize(
    (max: PropMaxSize.MaxSize, t: PropMaxSize.TestCases, r: RNG) =>
      run(max, t, r) match {
        case PropMaxSize.Passed => PropMaxSize.Passed
        case PropMaxSize.Proved => PropMaxSize.Proved
        case PropMaxSize.Falsified(failures, successes) => p.run(max, t, r)
      }
  )

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"gemerated an exception: ${e.getMessage} \n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
}
