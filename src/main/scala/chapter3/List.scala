package chapter3

/**
  * Created by lbajor on 2016-05-17.
  */
sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] = {
    def reverse(l: List[A]) = {
      @annotation.tailrec
      def reverseRec(l: List[A], result: List[A]): List[A] = l match {
        case Nil => result
        case Cons(x, xs) => reverseRec(xs, new Cons(x, result))
      }

      reverseRec(l, Nil)
    }

    @annotation.tailrec
    def go(acc: List[A], as: A*): List[A] = {
      if (as.isEmpty) acc
      else go(new Cons(as.head, acc), as.tail: _*)
    }

    reverse(go(Nil, as: _*))
  }

  //3.2
  def tail[A](l: List[A]) = l match {
    case Nil => throw new UnsupportedOperationException("Cannot get tail of empty list.")
    case Cons(h, rest) => rest
  }

  //3.3
  def setHead[A](l: List[A], newHead: A) = l match {
    case Nil => new Cons(newHead, Nil)
    case Cons(h, rest) => new Cons(newHead, rest)
  }

  //3.4
  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    require(n >= 0, "Cannot remove " + n + " elements from the list.")

    n match {
      case 0 => l
      case _ => l match {
        case Nil => throw new UnsupportedOperationException("Cannot remove elements from empty list.")
        case Cons(x, xs) => drop(xs, n - 1)
      }
    }
  }

  //3.5
  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(x, xs) =>
        if (f(x)) dropWhile(xs, f)
        else l
    }
  }

  //3.6
  def init[A](l: List[A]): List[A] = l match {
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
  def productFR(as: List[Double]) = foldRight(as, 1.0)(_ * _)

  //3.9
  def lengthFR[A](as: List[A]) =
    foldRight(as, 0)((l, len) => len + 1)

  //3.10
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    @annotation.tailrec
    def go(as: List[A], t: B): B = as match {
        case Nil => t
        case Cons(x, xs) => go(xs, f(t, x))
      }

    go(as, z)
  }

  //3.7 with short circuiting
  def lazyFoldRight[A, B](a: List[A], z: B)(f: (A, =>B) => B): B = a match {
    case Nil => z
    case Cons(x, xs) => f(x, lazyFoldRight(xs, z)(f))
  }

  //3.11
  def sumFL(l: List[Int]) = foldLeft(l, 0)(_+_)

  def productFL(l: List[Double]) = foldLeft(l, 1.0)(_*_)

  def lengthFL[A](l: List[A]) =
    foldLeft(l, 0)((len, l) => len+1)

  //3.12
  def reverseFL[A](l: List[A]) = foldLeft(l, Nil:List[A])((z, el) => new Cons(el, z))

  //3.13
  def foldLeftAsFR[A, B](as: List[A], z: B)(f: (B, A) => B): B = foldRight[A, B](reverseFL(as), z)((a, b) => f(b,a))
  def foldRightAsFL[A, B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft[A, B](reverseFL(as), z)((a, b) => f(b,a))

  //3.14
  def append[A](l: List[A], e: A): List[A] = l match {
    case Nil => Cons(e, l)
    case Cons(x, xs) => new Cons(x, append(xs, e))
  }

  //3.15
  def concatenate[A](l: List[List[A]]): List[A] = {
    foldRightAsFL(l, Nil: List[A])(
      (innerList, acc) => foldRightAsFL(innerList, acc)(
        (el, acc) => new Cons(el, acc)
      )
    )
  }

  //3.16
  def increase(l: List[Int], inc: Int): List[Int] = {
    l match {
      case Nil => l
      case Cons(x, xs) => Cons(x+inc, increase(xs, inc))
    }
  }

  //3.17
  def doubleToString(l: List[Double]): List[String] = {
    l match {
      case Nil => Nil
      case Cons(x, xs) => Cons(x.toString, doubleToString(xs))
    }
  }
  //3.18
  def map[A,B](as: List[A])(f: A => B): List[B] = foldRight(as, Nil:List[B])((el, z) => new Cons(f(el), z))

  //3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRight(as, Nil:List[A])((el, z) =>
    if (f(el)) new Cons(el, z)
    else z
  )
}
