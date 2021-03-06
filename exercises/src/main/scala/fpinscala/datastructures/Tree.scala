package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  // Exercise 3.25
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_)      => 1
    case Branch(l, r) => size(l) + 1 + size(r)
  }

  // Exercise 3.26
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v)      => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  // Exercise 3.27
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_)      => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  // Exercise 3.28
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v)      => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  // Exercise 3.29
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(v)      => f(v)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def size2[A](t: Tree[A]): Int = fold(t)(_ => 1)(_ + 1 + _)

  def maximum2(t: Tree[Int]): Int = fold(t)(identity)(_ max _)

  def depth2[A](t: Tree[A]): Int = fold(t)(_ => 0)((d1, d2) => 1 + (d1 max d2))

  def map2[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(v => Leaf(f(v)): Tree[B])(Branch(_, _))
}
