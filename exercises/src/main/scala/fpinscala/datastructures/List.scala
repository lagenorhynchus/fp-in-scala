package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil
    extends List[
      Nothing
    ] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int =
    ints match { // A function that uses pattern matching to add up a list of integers
      case Nil => 0 // The sum of the empty list is 0.
      case Cons(x, xs) =>
        x + sum(
          xs
        ) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
    }

  def product(ds: List[Double]): Double = ds match {
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // Exercise 3.1
  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _)))          => x
    case Nil                                   => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t)                            => h + sum(t)
    case _                                     => 101
  }
  // => 3

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil        => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](
      as: List[A],
      z: B
  )(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil         => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(
      _ * _
    ) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  // Exercise 3.2
  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil         => Nil
      case Cons(_, xs) => xs
    }
  // 入力が Nil の場合の実装上のその他の選択肢:
  // - エラー終了する(例外をスローする)
  // - 戻り値をOptionで包み、Noneを返す

  // Exercise 3.3
  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Nil         => Cons(h, Nil)
      case Cons(_, xs) => Cons(h, xs)
    }

  // Exercise 3.4
  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else
      l match {
        case Nil         => Nil
        case Cons(_, xs) => drop(xs, n - 1)
      }

  // Exercise 3.5
  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Nil                 => Nil
      case Cons(x, xs) if f(x) => dropWhile(xs, f)
      case _                   => l
    }

  // Exercise 3.6
  def init[A](l: List[A]): List[A] =
    l match {
      case Nil          => Nil
      case Cons(_, Nil) => Nil
      case Cons(x, xs)  => Cons(x, init(xs))
    }
  // このリストの実装は単方向連結リストであり、最終要素を除外したリストを得るにはリスト全体を走査し再構築する必要がある。

  // Exercise 3.7
  // この foldRight の実装では入力のリストを最終要素まで評価しなければならないため、特定の条件で再帰を中止して値を返すようなことはできない。

  // Exercise 3.8
  /*
  foldRight の引数 z に Nil 、f に Cons(_, _) を指定すると、もとのリストがそのまま得られる。

  foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)) == List(1, 2, 3)

  foldRight とはリストのデータコンストラクタ Nil, Cons を引数 z, f で置き換える操作と考えることができる。
   */

  // Exercise 3.9
  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, acc) => acc + 1)

  // Exercise 3.10
  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil         => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  // Exercise 3.11
  def sum3(ns: List[Int]): Int =
    foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Double]): Double =
    foldLeft(ns, 1.0)(_ * _)

  def length2[A](l: List[A]): Int = foldLeft(l, 0)((c, _) => c + 1)

  // Exercise 3.12
  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())((acc, x) => Cons(x, acc))

  // Exercise 3.13
  def foldLeft2[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(reverse(l), z)((x, y) => f(y, x))

  def foldRight2[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z)((x, y) => f(y, x))

  // Exercise 3.14
  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)(Cons(_, _))

  // Exercise 3.15
  def concat[A](ls: List[List[A]]): List[A] =
    foldRight(ls, Nil: List[A])(append)

  // Exercise 3.16
  def add1(ns: List[Int]): List[Int] =
    foldRight(ns, List[Int]())((x, xs) => Cons(x + 1, xs))

  // Exercise 3.17
  def doubleToString(ns: List[Double]): List[String] =
    foldRight(ns, Nil: List[String])((n, acc) => Cons(n.toString, acc))

  // Exercise 3.18
  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B])((x, acc) => Cons(f(x), acc))

  // Exercise 3.19
  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, Nil: List[A])((x, acc) => if (f(x)) Cons(x, acc) else acc)

  // Exercise 3.20
  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))

  // Exercise 3.21
  def filter2[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(x => if (f(x)) List(x) else Nil)

  // Exercise 3.22
  def addEach(a1: List[Int], a2: List[Int]): List[Int] =
    (a1, a2) match {
      case (Nil, _)                   => Nil
      case (_, Nil)                   => Nil
      case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, addEach(xs, ys))
    }

  // Exercise 3.23
  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] =
    (as, bs) match {
      case (Nil, _)                   => Nil
      case (_, Nil)                   => Nil
      case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
    }

  // Exercise 3.24
  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    sup match {
      case Nil                       => sub == Nil
      case _ if startsWith(sup, sub) => true
      case Cons(h, t)                => hasSubsequence(t, sub)
    }

  @annotation.tailrec
  private def startsWith[A](l: List[A], prefix: List[A]): Boolean =
    (l, prefix) match {
      case (_, Nil)                                 => true
      case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => startsWith(t1, t2)
      case _                                        => false
    }
}
