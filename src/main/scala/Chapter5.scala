package grieks.fpinscala.five
// Strictness and Laziness

import Stream._
trait Stream[+A] {

  // 5.1
  def toList: List[A] = {
    def loop(s: Stream[A], l: List[A]): List[A] = s match {
      case Cons(h,t) => h() :: loop(t(), l)
      case Empty => l
    }
    loop(this, List.empty)
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  // 5.2
  def take(n: Int): Stream[A] = {
    def loop(s: Stream[A], num: Int): Stream[A] = s match {
      case Cons(h,t) if num > 0 => cons(h(), loop(t(), num-1))
      case _ => empty
    }
    loop(this, n)
  }

  def drop(n: Int): Stream[A] = {
    def loop(s: Stream[A], num: Int): Stream[A] = s match {
      case Cons(h,t) if num > 0 => loop(t(), num-1)
      case _ => s
    }
    loop(this, n)
  }

  // 5.3
  def takeWhile(p: A => Boolean): Stream[A] = {
    def loop(s: Stream[A], f: A => Boolean): Stream[A] = s match {
      case Cons(h,t) if f(h()) => cons(h(), loop(t(), f))
      case _ => empty
    }
    loop(this, p)
  }

  // 5.4
  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h,t) if p(h()) => true && t().forAll(p)
    case Empty => true
    case _ => false
  }

  def headOption: Option[A] = sys.error("todo")

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def from(n: Int): Stream[Int] = sys.error("todo")

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = sys.error("todo")
}
