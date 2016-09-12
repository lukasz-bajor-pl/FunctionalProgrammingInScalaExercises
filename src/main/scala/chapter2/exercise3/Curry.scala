package pers.fpinscala.chapter2.exercise3

/**
  * Created by lbajor on 2016-05-08.
  */
object Curry {
  def curry[A, B, C](f: (A,B) => C): A => (B => C) = (a: A) => (b: B) =>  f(a,b)
}
