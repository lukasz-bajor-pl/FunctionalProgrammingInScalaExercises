package pers.fpinscala.chapter9

import pers.fpinscala.chapter8.{Gen, Prop}
import pers.fpinscala.chapter8.Prop._

import scala.util.matching.Regex

/**
  * Created by lukasb on 08/09/2016.
  */
trait Parsers[ParseError, Parser[+_]] { self =>
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))

  def or[A](s1: Parser[A], s2: =>Parser[A]): Parser[A]
  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p,p2)
    def or[B>:A](p2: =>Parser[B]): Parser[B] = self.or(p,p2)

    def many: Parser[List[A]] = self.many(p)
    def map[B](f: A=>B): Parser[B] = self.map(p)(f)

    def **[B](p2: Parser[B]): Parser[(A,B)] = self.product(p, p2)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)

    def exactly(n: Int) = self.exactly(p, n)

    def slice = self.slice(p)

    def product[B](p2: Parser[B]): Parser[(A,B)] = self.product(p, p2)
  }

  def slice[A](p: Parser[A]): Parser[String]
  def many1[A](p: Parser[A]): Parser[List[A]]

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
  def map2[A,B,C](p: Parser[A], p2: =>Parser[B])(f: (A,B) => C): Parser[C] = product(p, p2).map(f.tupled)


  def succeed[A](a: A): Parser[A] = string("") map (_ => a)

  //9.3
  def many[A](p: Parser[A]): Parser[List[A]] = {
    map2(p, many(p))({ case (a: A, lA: List[A]) => a :: lA }) or succeed(Nil:List[A])
  }

  //9.4
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = {
    require(n >= 0)

    if (n == 0) succeed(Nil:List[A])
    map2(p, listOfN(n-1, p))(_ :: _)
  }

  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  implicit def regex(r: Regex): Parser[String]

  def exactly[A](p: Parser[A], n: Int): Parser[String] = {
    require(n >= 0)

    if (n == 0) succeed("")
    else map2(p, exactly(p, n-1))(_ + _)
  }

  //9.6
  def digitsFolowedByChar(c: Char): Parser[String] = flatMap(regex("[0-9]*".r))(str => char(c) exactly str.toInt)

  //9.7
  def product[A,B](p: Parser[A], p2: =>Parser[B]): Parser[(A,B)] = flatMap(p)(a => p2 map(b => (a,b)))
  def map2AsFm[A,B,C](p: Parser[A], p2: =>Parser[B])(f: (A,B) => C): Parser[C] = flatMap(p)(a => p2 map(b => f(a, b)))

  //9.8
  def map[A,B](a: Parser[A])(f: A => B): Parser[B] = flatMap(a)(a => succeed(f(a)))

  //9.9
  def digit = regex("\\d".r).map(_.toInt)
}

trait JSON
object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON
}
