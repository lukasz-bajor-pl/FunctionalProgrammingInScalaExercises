package pers.fpinscala.chapter4

/**
  * Created by bajorl on 02/06/2016.
  */
sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(_) => this.asInstanceOf[Either[E, B]]
    case Right(e) => Right(f(e))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => this.asInstanceOf[Left[EE]]
    case Right(e) => f(e)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case Right(_) => this
  }

  def map2[EE >: E, B >: A, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    flatMap(a => b.map(bE => f(a, bE)))
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]


object Either {
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(identity(_))

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight((Right(Nil:List[B]):Either[E, List[B]]))(
      (el, res) => {
        f(el).map2(res)((e, l) => e :: l.asInstanceOf[List[B]])
      }
    )

  def map22[E, A, EE >: E, B, C](a: Either[E, A], b: Either[EE, B])(f: (A, B) => C): Either[List[EE], C] =
    a match {
      case Left(aErr) =>
        b match {
          case Left(bErr) => Left(List(aErr, bErr))
          case _ => Left(List(aErr))
        }

      case Right(aEl) =>
        b match {
          case Left(bErr) => Left(List(bErr))
          case Right(bEl) => Right(f(aEl,bEl))
        }
    }
}
