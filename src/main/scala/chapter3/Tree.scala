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

  //3.29
  def fold[A, B](t: Tree[A], z: Option[B])(f: (Option[B], Option[B], Tree[A])=>B): B = t match {
    case Leaf(v) => f(z, z, t)
    case Branch(l,r) => f(Some(fold(l, z)(f)), Some(fold(r, z)(f)), t)
  }

  def sizeF[A](t: Tree[A]): Int = fold(t, Some(0))((resA, resB, node) => resA.get + resB.get + 1)
  def maximumF(t: Tree[Int]): Int = fold(t, Some(Int.MinValue))((resA, resB, node) => node match {
    case Leaf(v) => resA.get.max(v)
    case _ => resA.get.max(resB.get)
  })
  def depthF[A](t: Tree[A]): Int = fold(t, Some(0))((resA, resB, node) => node match {
    case Leaf(v) => 1
    case _ => 1 + resA.get.max(resB.get)
  })
  def mapF[A, B](t: Tree[A])(f: A=>B): Tree[B] = fold(t, None:Option[Tree[B]])((resA, resB, node) => {
    node match {
      case Leaf(v) => Leaf(f(v))
      case _ => Branch(resA.get, resB.get)
    }
  })
}
