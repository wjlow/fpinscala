package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => l
    case Cons(_, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(_, xs) => Cons(h, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = (l, n) match {
    case (_, 0) => l
    case (Nil, _) => l
    case (Cons(_, xs), _) => drop(xs, n - 1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => l
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def length[A](l: List[A]): Int = l match {
    case Nil => 0
    case Cons(x, xs) => List.foldRight(xs, 1)((_, acc) => acc + 1)
  }

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sum3(ns: List[Int]) =
    foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _)

  def length2[A](l: List[A]): Int = l match {
    case Nil => 0
    case Cons(x, xs) => foldLeft(xs, 1)((acc, _ ) => acc + 1)
  }

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((a, b) => Cons(b, a))

  def append2[A](a1: List[A], a2: List[A]) =
    foldRight(a1, a2)(Cons(_, _))

  def concatListOfLists[A](ls: List[List[A]]): List[A] = foldRight(ls, List[A]())((a, b) => append(a, b))

  def mapPlusOne(ns: List[Int]): List[Int] =
    foldRight(ns, List[Int]())((a, b) => Cons(a+1, b))

  def mapDouble2String(ns: List[Double]): List[String] =
    foldRight(ns, List[String]())((a, b) => Cons(a.toString, b))

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, List[B]())((a, b) => Cons(f(a), b))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, List[A]())((a, b) => if (f(a)) Cons(a, b) else b)

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, List[B]())((h, t) => append(f(h), t))

  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) Cons(a, Nil) else Nil)

  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
    case (Nil, b) => b
    case (a, Nil) => a
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairwise(t1, t2))
  }

  def zipWith[A](a: List[A], b: List[A])(f: (A,A) => A): List[A] = (a, b) match {
    case (Nil, b) => b
    case (a, Nil) => a
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

}
