package chapter3

/**
  * Created by lbajor on 2016-05-27.
  */
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  //3.25
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(v) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  //3.26
  def maximum(t: Tree[Int]): Int = {
    def go(max: Int, t: Tree[Int]): Int = t match {
      case Leaf(v) => max.max(v)
      case Branch(l, r) => go(go(max, l), r)
    }

    go(Int.MinValue, t)
  }

  //3.27
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(v) => 1
    case Branch(l, r) => 1 + depth(l).max(depth(r))
  }

  //3.28
  def map[A,B](t: Tree[A])(f: A=>B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }
}
