package pers.fpinscala.chapter3

/**
  * Created by lbajor on 2016-05-17.
  */
object Chapter3 {
  def exercise1() = {
    scala.collection.immutable.List(1,2,3,4,5) match {
      case x :: 2 :: 4 :: _ => x
      case scala.collection.immutable.Nil => 42
      case x :: y :: 3 :: 4 :: _ => x+y
      case _ => 101
    }
  }
}
