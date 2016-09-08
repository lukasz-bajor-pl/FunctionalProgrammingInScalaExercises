package chapter9

import pers.fpinscala.chapter8.{Gen, Prop}
import pers.fpinscala.chapter8.Prop._

/**
  * Created by lukasb on 08/09/2016.
  */
trait Parsers[ParseError, Parser[+_]] { self =>
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))

  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]
  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p,p2)
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2)

    def many: Parser[List[A]] = self.many(p)
    def map[B](f: A=>B): Parser[B] = self.map(p)(f)

    def **[B](p2: Parser[B]): Parser[(A,B)] = self.product(p, p2)
  }

  def map[A,B](a: Parser[A])(f: A => B): Parser[B]
  def slice[A](p: Parser[A]): Parser[String]
  def many1[A](p: Parser[A]): Parser[List[A]]
  def product[A,B](p: Parser[A], p2: Parser[B]): Parser[(A,B)]

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)

    //9.2
    def productLaw[A,B](p1: Parser[A], p2: Parser[B])(in: Gen[String]): Prop =
      forAll(in)(s => (run(p1)(s), run(p2)(s)) == run(product(p1, p2))(s))
  }

  //9.1
  def map2[A,B,C](p: Parser[A], p2: Parser[B])(f: (A,B) => C): Parser[C] = product(p, p2).map({ case (a,b) => f(a, b) })


  def succeed[A](a: A): Parser[A] = string("") map (_ => a)

  //9.3
  def many[A](p: Parser[A]): Parser[List[A]] = {
    map2(p, or(many(p), succeed(Nil:List[A])))({ case (a: A, lA: List[A]) => a :: lA })
  }

  //9.4
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = {
    require(n >= 0)

    if (n == 0) succeed(Nil:List[A])
    map2(p, listOfN(n-1, p))
  }
}
