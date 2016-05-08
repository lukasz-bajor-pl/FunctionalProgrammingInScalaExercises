package chapter2.exercise1

/**
  * Created by lbajor on 2016-05-08.
  */
object Fib {
  def fib(n: Int): Int = {
    type FibValue = (Int, Int)

    @annotation.tailrec
    def tailRecFib(prePrevFib: FibValue, prevFib: FibValue)(n: Int): Int = {
      n match {
        case prePrevFib._1 => prePrevFib._2
        case prevFib._1 => prevFib._2
        case _ => tailRecFib(prevFib, (prevFib._1 + 1, prevFib._2 + prePrevFib._2))(n)
      }
    }

    tailRecFib ((0, 0), (1, 1)) (n)
  }
}
