package chapter2.exercise5

/**
  * Created by lbajor on 2016-05-08.
  */
object Compose {
  def compose[A,B,C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))
}
