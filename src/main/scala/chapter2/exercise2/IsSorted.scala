package pers.fpinscala.chapter2.exercise2

/**
  * Created by lbajor on 2016-05-08.
  */
object IsSorted {
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def isSortedRec(acc: Boolean, as: Stream[A]): Boolean = {
      if (acc == false) acc
      else if (as.length <= 1) acc
      else isSortedRec(acc && ordered(as(0), as(1)), as.tail)
    }

    isSortedRec(true, as.toStream)
  }
}
