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

  // 5.5
  def takeWhileF(p: A => Boolean): Stream[A] =
    foldRight(empty[A]: Stream[A])((a,b) => if (p(a)) cons(a,b) else empty)

  // 5.6
  def headOption: Option[A] =
    foldRight(None: Option[A])((a,b) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a,acc) => cons(f(a), acc))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((a,acc) => if (f(a)) cons(a, acc) else acc)

  // come back to this
  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)((a,acc) => cons(a, acc))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a,acc) => f(a).append(acc))

  // 5.13
  def mapU[B](f: A => B): Stream[B] = {
    unfold(this){
      case Cons(h,t) => Some((f(h()), t()))
      case _ => None
    }
  }

  def takeU(n: Int): Stream[A] = {
    unfold(this, n){
      case (Cons(h,t), num) if num > 0 => Some((h(), (t(), num-1)))
      case _ => None
    }
  }

  def takeWhileU(p: A => Boolean): Stream[A] = {
    unfold(this, p){
      case (Cons(h,t), f) if f(h()) => Some((h(), (t(), f)))
      case _ => None
    }
  }

  def zipWith[B,C](s: Stream[B])(f: (A,B) => C): Stream[C] = {
    unfold((this, s)){
      case (Cons(h1,t1), Cons(h2,t2)) => Some((f(h1(),h2()), (t1(), t2())))
      case _ => None
    }
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = {
    unfold((this, s2)){
      case (Cons(h1,t1), Cons(h2,t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
      case (Cons(h1,t1), Empty) => Some(((Some(h1()), None), (t1(), Empty)))
      case (Empty, Cons(h2,t2)) => Some(((None, Some(h2())), (Empty, t2())))
      case _ => None
    }
  }

  def startsWith[B](s: Stream[B]): Boolean = {
    (this.zipWith(s)((a,b) => if (a == b) b).toList == s.toList)
  }
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

  // 5.8
  def constant[A](a: A): Stream[A] =
    cons(a, constant(a))

  // 5.9
  def from(n: Int): Stream[Int] =
    cons(n, from(n+1))

  // 5.10
  def fibs: Stream[Int] = {
    def loop(n1: Int, n2: Int): Stream[Int] =
      cons(n1, loop(n2, n1+n2))
    loop(0,1)
  }

  // 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a,s)) => cons(a,unfold(s)(f))
      case None => empty
    }
  }

  // 5.12
  def fibsU: Stream[Int] =
    unfold((0,1)){ case (n1, n2) => Some((n1, (n2, n1 + n2))) }

  def fromU(i: Int): Stream[Int] =
    unfold(i)(n => Some((n, n+1)))

  def constantU[A](a: A): Stream[A] =
    unfold(a)(aa => Some((a,a)))

  def onesU: Stream[Int] =
    unfold(1)(n => Some((n,n)))

}
