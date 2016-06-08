package chapter5

/**
  * Created by bajorl on 08/06/2016.
  */
sealed trait Stream[+A] {
  def toList: scala.collection.immutable.List[A]

  def take(n: Int): Stream[A]
  def drop(n: Int): Stream[A]
  def takeWhile(p: A => Boolean): Stream[A]

  def foldRight[B](z: => B)(f: (A, =>B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def forAll(p: A=>Boolean): Boolean = foldRight(true)(
    (el, lazyBool) => p(el) && lazyBool
  )

  def takeWhileFoldRight(p: A=>Boolean): Stream[A] = foldRight(Empty:Stream[A])(
    (a, lazyB) => if (p(a)) Stream.cons(a, lazyB) else lazyB)

  def headOptionFR: Option[A] = foldRight(None:Option[A])(
    (h, lazyT) => Some(h)
  )

  def map[B](f: A=> B) = foldRight(Empty:Stream[B])(
    (h, t) => Stream.cons(f(h), t)
  )

  def filter(p: A=> Boolean) = foldRight(Empty:Stream[A])(
    (h, t) => if (p(h)) Stream.cons(h, t) else t
  )

  def append[B >: A](e: =>B): Stream[B] = foldRight(Stream.cons(e, Empty))(
    (h, t) => Stream.cons(h.asInstanceOf[B], t)
  )

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Empty:Stream[B])(
    (h, t) => f(h).foldRight(t)(
        (h1, t1) => Stream.cons(h1, t1)
      )
  )
}

case object Empty extends Stream[Nothing] {
  override val toList = Nil
  override def take(n: Int) = this
  override def drop(n: Int) = this
  override def takeWhile(p: Nothing => Boolean) = this
}

case class Cons[+A](h: ()=>A, t: ()=>Stream[A]) extends Stream[A] {
  override def toList = h() :: t().toList

  override def take(n: Int): Stream[A] = {
    if (n <= 0) Empty
    else {
      Stream.cons[A](h(), t().take(n-1))
    }
  }

  override def drop(n: Int): Stream[A] = {
    if (n<=0) this
    else t().drop(n-1)
  }

  override def takeWhile(p: A => Boolean) = {
    val hEvaluated = h()
    if (!p(hEvaluated)) Empty
    else {
      Stream.cons(hEvaluated, t().takeWhile(p))
    }
  }
}

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]) = {
    lazy val head = hd
    lazy val tail = tl

    Cons(() => head, () => tail)
  }

  def empty[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
}
