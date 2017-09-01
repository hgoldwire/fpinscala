package fpinscala.laziness

import fpinscala.laziness.Stream._

trait Stream[+A] {

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }


  def toListRecursive: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h() :: acc)
      case _ => acc
    }

    go(this, List()).reverse
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), Stream.empty)
    case _ => empty
  }

  def takeUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), 1) => Some((h(), (empty, 0)))
      case (Cons(h, t), n) if n > 1 => Some((h(), (t(), n - 1)))
      case _ => None
    }


  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t() takeWhile p)
    case _ => empty
  }

  def takeWhileUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _ => None
    }


  def takeWhileFR(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => {
      if (p(h))
        cons(h, t)
      else
        empty
    })

  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) && t().forAll(p)
    case _ => true
  }

  def headOption: Option[A] =
    foldRight(None: Option[A])((h, t) => Some(h))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  def mapUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if (f(h)) cons(h, t) else t)

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h) append t)

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(a, aa), Cons(b, bb)) => Some((f(a(), b()), (aa(), bb())))
      case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, s2)) {
      case (Cons(h, t), Cons(hh, tt)) => Some((Some(h()), Some(hh())), (t(), tt()))
      case (Cons(h, t), Empty) => Some((Some(h()), None: Option[B]), (t(), empty[B]))
      case (Empty, Cons(hh, tt)) => Some(((None: Option[A], Some(hh())), (empty[A], tt())))
      case (Empty, Empty) => None
    }

  def startsWith[B](s: Stream[B]): Boolean = (this, s) match {
    case (_, Empty) => true
    case (Empty, Cons(hh, tt)) => false
    case (Cons(h, t), Cons(hh, tt)) => if (h() == hh()) t().startsWith(tt()) else false
  }

  def startsWith2[B](s: Stream[B]): Boolean =
    zipAll(s).takeWhile(_._2.nonEmpty) forAll (a => a._1 == a._2)

  def tails: Stream[Stream[A]] =
    unfold(this) {
      //      case x@Cons(h, t) => Some(x, t())
      case Empty => None
      case s => Some(s, s drop 1)
    } append Stream(empty)

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)

  def scanRight[B](init: B)(f: Stream[A] => B): Stream[Stream[B]] =
    unfold((this, init)) {
      case (Empty, acc) => None
      case (s, acc) => Some(f(s) -> (s drop 1, f(s)))
    }


}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  val ones: Stream[Int] = Stream.cons(1, ones)

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  def fibs(p: Int = 0, pp: Int = 1): Stream[Int] =
    cons(p, fibs(pp, p + pp))


  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => empty
    }

  def fibsUnfold: Stream[Int] =
    unfold((0, 1))(s => Some((s._1, (s._2, s._1 + s._2))))

  def fromUnfold(n: Int): Stream[Int] =
    unfold(n)(s => Some((s, s + 1)))

  def constantUnfold[A](a: A): Stream[A] =
    unfold(a)(_ => Some((a, a)))

  def onesUnfold: Stream[Int] = constantUnfold(1)

}