package grieks.fpinscala

import scala.{Option => _, Some => _, Either => _, Left => _, Right => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {

  // 4.1
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

  // TODO: no pattern matching
  def orElse[B>:A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case Some(_) => this
  }

  // TODO: no pattern matching
  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // 4.2
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  // 4.3
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(a1 => b.map(b2 => f(a1,b2)))

  // 4.4
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Cons(head,tail) => head.flatMap(h => sequence(tail).map(t => Cons(h,t)))
    case Nil => Some(Nil)
  }

  // 4.5
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Cons(head,tail) => f(head).flatMap(h => traverse(tail)(f).map(t => Cons(h,t)))
    case Nil => Some(Nil)
  }

  def sequenceT[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(opt => opt)
}

// ------------------
// Eithers
// ------------------

sealed trait Either[+E,+A] {

  // 4.6
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => b
    case Right(a) => Right(a)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    this.flatMap(a => b.map(b2 => f(a,b2)))
}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = es match {
    case Cons(head,tail) => f(head).flatMap(h => traverse(tail)(f).map(t => Cons(h,t)))
    case Nil => Right(Nil)
  }

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] =
    traverse(es)(e => e)

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

}
