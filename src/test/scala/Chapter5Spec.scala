package grieks.fpinscala.five

import org.specs2.mutable.Specification
import org.specs2.specification.Scope

object Chapter5Spec extends Specification {
  import Stream._

  "Exercise 5.1" should {
    "convert a Stream to a List" in new context {
      intStream.toList must_== List(1,2,3)
    }

    "convert an Empty Stream to an empty List" in new context {
      Empty.toList must_== List.empty
    }
  }

  "Exercise 5.2" should {
    "return a Stream containing the provided number of elements using take" in new context {
      intStream.take(2).toList must_== List(1,2)//Cons(() => 1, Cons(() => 2, () => Empty))
    }

    "return Empty when taking 0" in new context {
      intStream.take(0) must_== Empty
    }

    "return Empty when taking less than 0" in new context {
      intStream.take(-1) must_== Empty
    }

    "return a Stream without the provided number of elements using drop" in new context {
      intStream.drop(2).toList must_== List(3)
    }

    "return the original Stream when dropping 0" in new context {
      intStream.drop(0) must_== intStream
    }

    "return the original Stream when dropping less than 0" in new context {
      intStream.drop(-1) must_== intStream
    }
  }

  "Exercise 5.3" should {
    "take all elements while predicate is true" in new context {
      intStream.takeWhile(_ < 3).toList must_== List(1,2)
    }

    "not take elements that meet predicate, following elements that don't" in new context {
      Stream(1,3,2).takeWhile(_ < 3).toList must_== List(1)
    }

    "return Empty Stream if predicate is not met" in new context {
      intStream.takeWhile(_ < 1) must_== Empty
    }
  }

  trait context extends Scope {
    lazy val intStream = Stream(1,2,3) // Stream(() => 1, () => 2, () => 3)
    //lazy val emptyInt: Stream[Int] = empty[Int]
    //val consStream: Stream[Int] = Cons(() => 3, () => emptyInt)
  }
}
