package chapter3

/**
  * Created by lbajor on 2016-05-17.
  */
object Chapter3 {
  def exercise1() = {
    List(1,2,3,4,5) match {
      case x :: 2 :: 4 :: _ => x
      case Nil => 42
      case x :: y :: 3 :: 4 :: _ => x+y
      case _ => 101
    }
  }
}
