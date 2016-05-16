package chapter3

/**
  * Created by lbajor on 2016-05-17.
  */
sealed trait List[+A] {
  def tail: List[A]
}

case object Nil extends List[Nothing] {
  override def tail: List[Nothing] = throw new UnsupportedOperationException("Cannot get tail of empty list.")
}

case class Cons[+A](head: A, override val tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}
