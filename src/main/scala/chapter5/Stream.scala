package pers.fpinscala.chapter5

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

  def mapUnfold[B](f: A=> B) = Stream.unfold(this)(a => a match {
    case Empty => None
    case Cons(h,t) =>
      Some((f(h()), t()))
  })

  def takeUnfold(n: Int): Stream[A] =
    Stream.unfold((n, this))({
      case (n, s) =>
        if (n <= 0) None
        else s match {
          case Empty => None
          case Cons(h, t) => Some(h(), (n-1, t()))
        }
    })

  def takeWhileUnfold(p: A => Boolean): Stream[A] =
    Stream.unfold(this)(_ match {
      case Empty => None
      case Cons(h, t) =>
        val hEv = h()
        if (!p(hEv)) None
        else Some((hEv, t()))
    })

  def zipWith[B,C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    Stream.unfold((this, s2))(_ match {
      case (Empty, _) => None
      case (_, Empty) => None
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some(f(h1(), h2()), (t1(), t2()))
    })

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    Stream.unfold((this, s2))(_ match {
      case (Empty, Empty) => None
      case (Empty, Cons(h, t)) => Some((None, Some(h())), (Empty, t()))
      case (Cons(h, t), Empty) => Some((Some(h()), None), (t(), Empty))
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
    })

  def startsWith[B >: A](s: Stream[B]): Boolean = zipAll(s).forAll({case (a, b) => b == None || a == b })

  def tails: Stream[Stream[A]] = Stream.unfold(Some(this):Option[Stream[A]])(_ match {
    case None => None
    case Some(Empty) => Some(Empty:Stream[A], None)
    case Some(c@Cons(h, t)) => Some(c, Some(t()))
  })

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def hasSubsequence[B >: A](s2: Stream[B]): Boolean = tails exists(_ startsWith s2)

  def find(p: A => Boolean): Option[A]
}

case object Empty extends Stream[Nothing] {
  override val toList = Nil
  override def take(n: Int) = this
  override def drop(n: Int) = this
  override def takeWhile(p: Nothing => Boolean) = this
  override def find(p: Nothing => Boolean) = None:Option[Nothing]
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

  override def find(p: A => Boolean) : Option[A] = {
    lazy val hEval = h()

    if (p(hEval)) Some(hEval)
    else t().find(p)
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

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n+1))

  def fibs: Stream[Int] = {
    def fibsFrom(a: Int, b: Int): Stream[Int] = cons(a, fibsFrom(b, a+b))

    fibsFrom(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z).map(e => cons(e._1, unfold(e._2)(f))).getOrElse(Empty:Stream[A])

  def fibsUnfold: Stream[Int] = unfold((0,1))({ case (a,b) => Some((a, (b, a+b))) })
  def fromUnfold(n: Int): Stream[Int] = unfold(n)(n => Some((n, n+1)) )
  def constantUnfold[A](a: A): Stream[A] = unfold(a)(a => Some((a, a)) )

}