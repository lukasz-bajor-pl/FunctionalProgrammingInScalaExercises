package chapter3

/**
  * Created by lbajor on 2016-05-17.
  */
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  //3.3
  def tail[A](l: List[A]) = l match {
    case Nil => throw new UnsupportedOperationException("Cannot get tail of empty list.")
    case Cons(h, rest) => rest
  }

  //3.4
  def drop[A](l: List[A], n: Int): List[A] = {
    require(n >= 0, "Cannot remove " + n + " elements from the list.")

    n match {
      case 0 => l
      case _ => l match {
        case Nil => throw new UnsupportedOperationException("Cannot remove elements from empty list.")
        case Cons(x, xs) => drop(xs, n-1)
      }
    }
  }

  //3.5
  def setHead[A](l: List[A], newHead: A) = l match {
    case Nil => new Cons(newHead, Nil)
    case Cons(h, rest) => new Cons(newHead, rest)
  }

  //3.6
  def init[A](l: List[A]) : List[A] = l match {
    case Nil => throw new UnsupportedOperationException("Cannot remove last element from empty list.")
    case Cons(x, xs) => xs match {
      case Nil => Nil
      case _ => new Cons(x, init(xs))
    }
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  //3.7 - don't know how to short-cirtut once element is 0
  def productFR(as: List[Double]) = foldRight(as, 1.0) (_*_)

  //3.9
//  def lengthFR[A](as: List[A]) = ???
}
