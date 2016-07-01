package pers.fpinscala.chapter7

import java.util.concurrent.ExecutorService

/**
  * Created by bajorl on 27/06/2016.
  */
object Chapter7 {
  def sum(es: ExecutorService)(ints: Seq[Int]): Int =
    if (ints.size <= 1)
      ints.headOption getOrElse 0
    else {
      val (l, r) = ints.splitAt(ints.length / 2)

      val asyncSubSum = Par.asyncF(sum(es))

      val res = Par.map2(
        Par.fork(asyncSubSum(l)),
        Par.fork(asyncSubSum(r)))(_ + _)

      Par.run(es)(res) match {
        case Left(result) => result
        case Right(ex) => throw ex
      }
    }

  //7.6
  def generalParFunc[A](es: ExecutorService, f: (A,A) => A, zA: A)(as: Seq[A]): A = {
    if (as.size <= 1)
      as.headOption.map(e => f(e, zA)).getOrElse(zA)
    else {
      val (l, r) = as.splitAt(as.length / 2)

      val asyncSub = Par.asyncF(generalParFunc(es, f, zA))

      val res = Par.map2(
        Par.fork(asyncSub(l)),
        Par.fork(asyncSub(r)))(f(_, _))

      Par.run(es)(res) match {
        case Left(result) => result
        case Right(ex) => throw ex
      }
    }
  }

  def max(es: ExecutorService)(as: Seq[Int]) = generalParFunc(es, (a: Int, b: Int) => a.max(b), Int.MinValue)(as)

  def generalParAndMapFunc[A, B](es: ExecutorService, f: A => B, zB: B, g: (B, B) => B)(as: Seq[A]): B = {
    if (as.size <= 1)
      as.headOption.map(e => f(e)).getOrElse(zB)
    else {
      val (l, r) = as.splitAt(as.length / 2)

      val asyncSub = Par.asyncF(generalParAndMapFunc(es, f, zB, g))

      val res = Par.map2(
        Par.fork(asyncSub(l)),
        Par.fork(asyncSub(r)))(g)

      Par.run(es)(res) match {
        case Left(result) => result
        case Right(ex) => throw ex
      }
    }
  }

  def countWords(es: ExecutorService)(as: Seq[String]) =
    generalParAndMapFunc(es, (a: String) => a.split(" ").length, 0, (a: Int, b: Int) => a+b)(as)
}
