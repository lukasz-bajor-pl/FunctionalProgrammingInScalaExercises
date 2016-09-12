package pers.fpinscala.chapter2.exercise4

/**
  * Created by lbajor on 2016-05-08.
  */
object Uncurry {
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)
}
