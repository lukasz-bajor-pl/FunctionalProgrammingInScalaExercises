package pers.fpinscala.chapter2.exercise1

/**
  * Created by lbajor on 2016-05-08.
  */
object Fib {
  def fib(n: Int): Int = {
    @annotation.tailrec
    def tailRecFib(prePrevFib: Int, prevFib: Int, n: Int): Int = {
      n match {
        case 0 => prePrevFib
        case _ => tailRecFib(prevFib, prePrevFib+prevFib, n-1)
      }
    }

    tailRecFib (0, 1, n)
  }
}
