package chapter4

/**
  * Created by bajorl on 02/06/2016.
  */
object Chapter4 {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(e => math.pow(e - m, 2))))

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a.flatMap(aEl => b.map(bEl => f(aEl, bEl)))

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight(Some(Nil: List[A]): Option[List[A]])(
      (elOption, res) => map2(elOption, res)((a,b) => a::b)
    )

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight(Some(Nil: List[B]): Option[List[B]])(
      (elA, result) => map2(f(elA), result)((a,b) => a::b)
    )

  def sequenceAsTraverse[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(identity(_))
}
