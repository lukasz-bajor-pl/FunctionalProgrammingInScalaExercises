package pers.fpinscala.chapter8

import java.util.concurrent.{Executors, ExecutorService}

import pers.fpinscala.chapter7.Par
import pers.fpinscala.chapter7.Par._

/**
  * Created by bajorl on 13/07/2016.
  */
object Chapter8 {
  val random0To100 = Gen.choose(0, 100)
  val intList = random0To100.listOfN(random0To100)
  val prop = Prop.forAll(intList)(ns => ns.reverse.reverse == ns) &&
    Prop.forAll(intList)(ns => ns.headOption == ns.reverse.lastOption)

  val failingProp = Prop.forAll(intList)(ns => ns.reverse == ns)

  //8.2
  val nonEmptySize = Gen.choose(1, 100)
  val maxList = random0To100.listOfN(nonEmptySize)
  val maxProp82 = Prop.forAll(maxList)(ns => (ns.max == ns.head || ns.max == ns.tail.max)) &&
  Prop.forAll(maxList)(ns => ns.max == ns.reverse.max)

  //
  val smallInt = Gen.choose(-10, 10)
  val maxProp = PropMaxSize.forAll(SGen.listOf(smallInt)) { ns =>
    val max = ns.max
    !ns.exists(_ > max)
  }

  //8.13
  val maxProp1 = PropMaxSize.forAll(SGen.listOf1(smallInt)) { ns =>
    val max = ns.max
    !ns.exists(_ > max)
  }

  //8.14
  val sortedProp = PropMaxSize.forAll(SGen.listOf1(smallInt)) {
    ns =>
      def isSorted(l: List[Int]): Boolean = l match {
        case Nil => true
        case h :: tail => h == l.min && isSorted(tail)
      }

      isSorted(ns.sorted)
  }

  object ** {
    def unapply[A, B](p: (A, B)) = Some(p)
  }

  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] =
    Par.map2(p, p2)(_ == _)

  def forAllPar[A](S: Gen[ExecutorService])(g: Gen[A])(f: A => Par[Boolean]): Prop =
    Prop.forAll(S ** g) { case s ** a => Par.run[Boolean](s)(f(a)) match {
      case Left(result) => result
      case Right(ex) => false
    }
    }

  def checkPar(ES: ExecutorService)(p: => Par[Boolean]): PropMaxSize = PropMaxSize(
    (_,_,_) => (Par.run(ES)(p)) fold(
      if (_) PropMaxSize.Passed else PropMaxSize.Falsified("()", 0),
      e => PropMaxSize.Falsified(e.getMessage, 0)
      )
  )

  val pint = Gen.choose(0, 10) map (Par.unit(_))

  //8.16
  val pint2 = Gen.choose(-20,20).listOfN(Gen.choose(0, 10)).map(l =>
    l.foldLeft(Par.unit(0))((b, i) => Par.fork{
      Par.map2(b, Par.unit(i))(_+_)
    }))

  //8.18
  val takeWhileProp = (p: Int => Boolean) => PropMaxSize.forAll(SGen.listOf(Gen.choose(0, 10))) {
    ns => ns == ns.takeWhile(p) ::: ns.dropWhile(p)
  }

  //8.19
  def genFunc[A, B](g: Gen[A])(f: (A, B) => A): Gen[B => A] = g.map(a => b => f(a,b))
  def genStrinfIntFn(g: Gen[Int]): Gen[String => Int] = genFunc(g)({ case (i, s) => i % s.hashCode })
}
