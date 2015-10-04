package grieks.fpinscala

// 3.1
// case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
// 3

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

  // 3.2
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil // should this be allowed?
    case Cons(_, xs) => xs
  }

  // 3.3
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => Cons(h, xs)
  }

  // 3.4
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0) l
    else l match {
      case Nil => Nil
      case Cons(_, xs) => drop(xs, n-1)
    }
  }

  // 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil // unnecessary
    case Cons(x, xs) => {
      if (f(x)) dropWhile(xs, f)
      else l
    }
  }

  // 3.6
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  // 3.7
  // No, foldRight will traverse all the way to the end of the List:
  // "Note that foldRight must traverse all the way to the end of the list (pushing frames
  // onto the call stack as it goes) before it can begin collapsing it"

  // 3.8
  // scala> List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
  // res0: grieks.fpinscala.List[Int] = Cons(1,Cons(2,Cons(3,Nil)))
  // same list

  // 3.9
  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_,y) => y + 1)

  // 3.10
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z,x))(f)
    }
  }

  // 3.11
  def sumLeft(ns: List[Int]): Int =
    foldLeft(ns, 0)(_ + _)

  def productLeft(ns: List[Double]): Double =
    foldLeft(ns, 1.0)(_ * _)

  def lengthLeft[A](as: List[A]): Int =
    foldLeft(as, 0)((x,_) => x + 1)

  // 3.12
  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, Nil: List[A])((acc,a) => Cons(a, acc))

  def reverseR[A](as: List[A]): List[A] =
    foldRight(as, Nil: List[A])((a,acc) => append(acc, Cons(a, Nil)))

  // 3.13
  def foldRightL[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((b,a) => f(a, b))

  // 3.14
  def appendR[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)((a,acc) => Cons(a, acc))

  // 3.15
  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil:List[A])(append)

  // 3.16
  def transform(nums: List[Int]): List[Int] =
    foldRight(nums, Nil: List[Int])((a,acc) => Cons(a+1, acc))

  // 3.17
  def asString(dubs: List[Double]): String =
    foldLeft(dubs, "")(_.toString + _)

  // 3.18
  def map[A,B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil: List[B])((a,acc) => Cons(f(a), acc))

  // 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((a,acc) => if (f(a)) Cons(a, acc) else acc)

  // 3.20
  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))

  // 3.21
  def filterWithFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  // 3.22
  def addContents(a1: List[Int], a2: List[Int]): List[Int] = {
    if (length(a1) == length(a2)) (a1, a2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(x1, xs1), Cons(x2, xs2)) => Cons(x1+x2, addContents(xs1,xs2))
    }
    else Nil
  }

  // 3.23
  def zipWith[A,B](a1: List[A], a2: List[A])(f: (A, A) => B): List[B] = {
    if (length(a1) == length(a2)) (a1, a2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(x1, xs1), Cons(x2, xs2)) => Cons(f(x1,x2), zipWith(xs1,xs2)(f))
    }
    else Nil
  }

  // 2.24
  // TODO


}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  // 3.25
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l,r) => (size(l) + size(r)) + 1
  }

  // 3.26
  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(n) => n
    case Branch(l,r) => maximum(l) max maximum(r)
  }

  // 3.27
  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 0
    case Branch(l,r) => (depth(l) max depth(r)) + 1
  }

  // 3.28
  def map[A,B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(n) => Leaf(f(n))
    case Branch(l,r) => Branch(map(l)(f), map(r)(f))
  }

  // 3.29
  def fold[A,B](tree: Tree[A])(lf: A => B)(bf: (B,B) => B): B = tree match {
    case Leaf(n) => lf(n)
    case Branch(l,r) => bf(fold(l)(lf)(bf), fold(r)(lf)(bf))
  }

  def sizeF[A](tree: Tree[A]): Int =
    fold(tree)(n => 1)((l,r) => 1 + (l + r))

  def maximumF(tree: Tree[Int]): Int =
    fold(tree)(n => n)(_ max _)

  def depthF[A](tree: Tree[A]): Int =
    fold(tree)(n => 0)((l,r) => 1 + (l max r))

  def mapF[A,B](tree: Tree[A])(f: A => B): Tree[B] =
    fold(tree)(n => Leaf(f(n)): Tree[B])((l,r) => Branch(l,r))

}
