package pers.fpinscala.chapter4

/**
  * Created by bajorl on 02/06/2016.
  */
sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(el) => Some(f(el))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(e) => e
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = map(x => Some(x.asInstanceOf[B])).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] = flatMap(x => if (f(x)) this else None)
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]
