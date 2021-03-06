package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`

case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def tail[A](l: List[A]): List[A] = drop(l, 1)

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(head, tail: List[A]) => Cons(h, tail)
  }

  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Cons(h, t: List[A]) if n > 0 => drop(t, n - 1)
    case _ => l
  }

  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t: List[A]) if f(h) => dropWhile(t, f)
    case _ => l
  }

  @annotation.tailrec
  def dropWhile2[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(h, t: List[A]) if f(h) => dropWhile2(t)(f)
    case _ => l
  }


  def init[A](l: List[A]): List[A] = {
    @annotation.tailrec
    def go(newL: List[A], origL: List[A]): List[A] = {
      origL match {
        case Nil => Nil
        case Cons(_, Nil) => newL
        case Cons(h, t: List[A]) => go(List.append(newL, Cons(h, Nil)), t)
      }
    }

    go(Nil, l)
  }

  def init2[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("init of empty list")
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

  def length[A](l: List[A]): Int = foldRight(l, 0)((a, b) => b + 1)

  def length2[A](l: List[A]): Int = foldLeft(l, 0)((a, b) => a + 1)

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t: List[A]) => foldLeft(t, f(z, h))(f)
  }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def sum3(ns: List[Int]) = foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)

  def length3[A](ns: List[A]) = foldLeft(ns, 0)((b, _) => b + 1)

  def reverse[A](ns: List[A]) = foldLeft(ns, Nil: List[A])((acc, cur) => Cons(cur, acc))

  //  def append3[A](a1: List[A], a2: List[A]): List[A] = foldLeft()()


  //  def foldLeftAppend[A](xs: List[A], a: A) = foldLeft(xs, Cons(a, Nil))((acc, cur) => foldLeftAppend(acc, cur))

  def add1(l: List[Int]): List[Int] = foldRight(l, Nil: List[Int])((h, t) => Cons(h + 1, t))

  def doubleToString(l: List[Double]): List[String] = foldRight(l, Nil: List[String])((a, b) => Cons(a.toString, b))

  def map[A, B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil: List[B])((h, t) => Cons(f(h), t))

  def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRight(as, Nil: List[A])((h, t) => {
    if (f(h))
      Cons(h, t)
    else
      t
  })

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = foldRight(as, Nil: List[B])((a, b) => append(f(a), b))

  def filterFlatMap[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(a => {
    if (f(a))
      List(a)
    else
      Nil
  })

  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairwise(t1, t2))
  }

  def zipWith[A,B,C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }


}
