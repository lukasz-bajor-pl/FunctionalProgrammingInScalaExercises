package pers.fpinscala.chapter6

/**
  * Created by bajorl on 16/06/2016.
  */
case class State[S,+A](run: S => (A,S)) {
  //6.10
  def flatMap[B](g: A => State[S, B]): State[S, B] = {
    new State(s => {
      val (a, nS) = run(s)
      g(a).run(nS)
    })
  }

  def map[B](f: A => B): State[S, B] = flatMap(a => State(s => (f(a), s)))
  def map2[B, C](o: State[S, B])(f: (A, B) => C): State[S, C] = flatMap(a => State(s => {
    val (b, nS) = o.run(s)
    (f(a, b), nS)
  }))
}

object State {
  //6.10
  def unit[A, S](a: A): State[S, A] = new State((s: S) => (a, s))

  def get[S]: State[S, S] = State(s => (s, s))
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S=>S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()
}
