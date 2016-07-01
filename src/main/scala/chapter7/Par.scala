package pers.fpinscala.chapter7

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent._

/**
  * Created by bajorl on 27/06/2016.
  */

object Par {
  type EX = Exception

  sealed trait Future[A] {
    private[chapter7] def apply(k: Either[A, EX] => Unit): Unit
  }

  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = (es: ExecutorService) => new Future[A] {
    override private[chapter7] def apply(k: (Either[A, EX]) => Unit): Unit = k(Left(a))
  }

  def exception[A](ex: Exception): Par[A] = (es: ExecutorService) => new Future[A] {
    override private[chapter7] def apply(k: (Either[A, EX]) => Unit): Unit = k(Right(ex))
  }

  def fork[A](a: => Par[A]): Par[A] = es => new Future[A] {
    def apply(k: (Either[A, EX]) => Unit) = eval(es)(a(es)(k))
  }

  def eval[A](es: ExecutorService)(r: => Unit): Unit =
    es.submit(new Callable[Unit] { def call = r })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](ex: ExecutorService)(p: Par[A]): Either[A, EX] = {
    val ref = new AtomicReference[Either[A, EX]]()
    val latch = new CountDownLatch(1)

    p(ex) { a => ref.set(a); latch.countDown() }
    latch.await()
    ref.get
  }

  //7.10
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = es => new Future[C] {
    def apply(k: (Either[C, EX]) => Unit) = {
      var ar: Option[Either[A, EX]] = None
      var br: Option[Either[B, EX]] = None

      def exec(a: Either[A, EX], b: Either[B, EX]): Unit = {
        (a, b) match {
          case (Left(ai), Left(bi)) => eval(es)(k(Left(f(ai, bi))))
          case (Right(exA), _) => eval(es)(k(Right(exA)))
          case (_, Right(exB)) => eval(es)(k(Right(exB)))
        }
      }

      val combiner = Actor[Either[Either[A, EX], Either[B, EX]]](es)(
        msg => msg match {
          case Left(a) => br match {
            case None => ar = Some(a)
            case Some(b) => exec(a, b)
          }

          case Right(b) => ar match {
            case None => br = Some(b)
            case Some(a) => exec(a, b)
          }
        }
      )

      a(es)(a => combiner ! Left(a))
      b(es)(b => combiner ! Right(b))
    }
  }

  //TODO map2 with timeout

  //7.4
  def asyncF[A, B](f: A => B): A => Par[B] = (a: A) => lazyUnit(f(a))

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  //7.5
  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    ps.foldLeft(unit(Nil: List[A]))({ case (r, p) =>
      map2(r, p)({ case (re, el) => el :: re }) })
  }

  //7.6
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val sRes = as.map(e => lazyUnit(e))

    sRes.foldLeft(unit(Nil: List[A]))({
      case (r, p) => map2(r, p)({
        case (re, el) =>
          if (f(el)) el :: re
          else re
      })
    })
  }

  def map3[A, B, C, D](a: Par[A], b: Par[B], c: Par[C])(f: (A, B, C) => D): Par[D] = {
    map2(
      map2(a, b)((_,_)),
      c
    )({
      case ((a, b), c) => f(a, b, c)
    })
  }

  def map4[A, B, C, D, E](a: Par[A], b: Par[B], c: Par[C], d: Par[D])(f: (A, B, C, D) => E): Par[E] = {
    map2(
      map2(
        map2(a, b)((_, _)),
        c)({ case ((a,b), c) => (a, b, c)}),
      d
    )({
      case ((a, b, c), d) => f(a, b, c, d)
    })
  }

  def map5[A, B, C, D, E, F](a: Par[A], b: Par[B], c: Par[C], d: Par[D], e: Par[E])(f: (A, B, C, D, E) => F): Par[F] = {
    map2(
      map2(
        map2(
          map2(a, b)((_, _)),
          c)({ case ((a, b), c) => (a, b, c) }),
        d)({
        case ((a, b, c), d) => (a, b, c, d)
      }),
      e)({
      case ((a, b, c, d), e) => f(a, b, c, d ,e)
    })
  }

  //7.11
  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = {
    es => run(es)(n) match {
      case Left(i) => choices.drop(i).head(es)
      case Right(ex) => exception[A](ex)(es)
    }
  }

  def choice[A](n: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = {
    choiceN(map(n)(b => if (b) 0 else 1))(List(t, f))
  }

  //7.12
  def choiceMap[K, V](k: Par[K])(m: Map[K, Par[V]]): Par[V] = {
    es => run(es)(k) match {
      case Left(key) => m(key)(es)
      case Right(ex) => exception[V](ex)(es)
    }
  }

  def flatMap[A, B](k: Par[A])(choices: A => Par[B]): Par[B] = {
    es => run(es)(k).fold(
      choices(_)(es),
      exception[B](_)(es)
    )
  }

  def choiceMapAsChoser[K, V](k: Par[K])(m: Map[K, Par[V]]): Par[V] = flatMap(k)(m(_))
  def choiceNAsChoser[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = flatMap(n)(choices.drop(_).head)
  def choiceAsChoser[A](n: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = flatMap(n)(if (_) t else f)

  //7.13
  def join[A](a: Par[Par[A]]): Par[A] = es => run(es)(a).fold(_(es), exception[A](_)(es))
  def fMAsJoin[A, B](k: Par[A])(choices: A => Par[B]): Par[B] = join(map(k)(choices))
  def joinAsFM[A](a: Par[Par[A]]): Par[A] = flatMap(a)(identity)

  //post 7.14
  //sequential execution
  def sequentialMap2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = flatMap(a)(a => flatMap(b)(b => unit(f(a, b))))

  //laws for join
  // join(unit(unit(x))) = unit(x) => join(unit(x)) = x

  //don't know if producer / consumer is representible
}
