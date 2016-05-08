package chapter2.exercise1

/**
  * Created by lbajor on 2016-05-08.
  */
object Fib {
  def fib(n: Int): Int =
    n match {
      case 0 => 0
      case 1 => 1
      case _ => fib(n-1) + fib(n-2)
    }
}
