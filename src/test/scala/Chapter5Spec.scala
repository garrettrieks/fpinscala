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

  "Exercise 5.4" should {
    "return True if all elements meet the predicate" in new context {
      intStream.forAll(_ > 0) must beTrue
    }

    "return False if not all elements meet the predicate" in new context {
      intStream.forAll(_ < 2) must beFalse
    }
  }

  "Exercise 5.5" should {
    "take all elements while predicate is true" in new context {
      intStream.takeWhileF(_ < 3).toList must_== List(1,2)
    }

    "not take elements that meet predicate, following elements that don't" in new context {
      Stream(1,3,2).takeWhileF(_ < 3).toList must_== List(1)
    }

    "return Empty Stream if predicate is not met" in new context {
      intStream.takeWhileF(_ < 1) must_== Empty
    }
  }

  "Exercise 5.6" should {
    "return the head of a Stream" in new context {
      intStream.headOption must beSome(1)
    }

    "return None for an Empty Stream" in new context {
      Stream().headOption must beNone
    }
  }

  "Exercise 5.7" should {
    "map a Stream using foldRight" in new context {
      intStream.map(_+1).toList must_== List(2,3,4)
    }

    "filter a Stream using foldRight" in new context {
      intStream.filter(_ > 1).toList must_== List(2,3)
    }

    "append a Stream using foldRight" in new context {
      intStream.append(Stream(4,5)).toList must_== List(1,2,3,4,5)
    }

    "flatMap a Stream using foldRight" in new context {
      intStream.flatMap(a => Stream(a*a)).toList must_== List(1,4,9)
    }
  }

  "Exercise 5.8" should {
    "generate a stream of constant values" in new context {
      constant(0).take(5).toList must_== List(0,0,0,0,0)
    }
  }

  "Exercise 5.9" should {
    "generate a stream of integers from the starting integer" in new context {
      from(1).take(5).toList must_== List(1,2,3,4,5)
    }
  }

  "Exercise 5.10" should {
    "generate a stream of fibonacci numbers" in new context {
      fibs.take(7).toList must_== List(0,1,1,2,3,5,8)
    }
  }

  "Exercise 5.12" should {
    "generate a stream of fibonacci numbers using unfold" in new context {
      fibsU.take(7).toList must_== List(0,1,1,2,3,5,8)
    }

    "generate a stream of integers from the starting integer using unfold" in new context {
      fromU(1).take(5).toList must_== List(1,2,3,4,5)
    }

    "generate a stream of constant values using unfold" in new context {
      constantU(0).take(5).toList must_== List(0,0,0,0,0)
    }

    "generate a stream of ones using unfold" in new context {
      onesU.take(5).toList must_== List(1,1,1,1,1)
    }
  }

  "Exercise 5.13" should {
    "map a Stream using unfold" in new context {
      intStream.mapU(_+1).toList must_== List(2,3,4)
    }

    "take n elements from a Stream using unfold" in new context {
      intStream.takeU(2).toList must_== List(1,2)//Cons(() => 1, Cons(() => 2, () => Empty))
    }

    "take 0 elements from a stream using unfold" in new context {
      intStream.takeU(0) must_== Empty
    }

    "take the first elements that meet predicate using unfold" in new context {
      Stream(1,3,2).takeWhileU(_ < 3).toList must_== List(1)
    }

    "don't take any if predicate is not met using unfold" in new context {
      intStream.takeWhileU(_ < 1) must_== Empty
    }

    "zipWith a Stream using unfold" in new context {
      intStream.zipWith(intStream)((a,b) => a+b).toList must_== List(2,4,6)
    }

    "zipAll a Stream with less elements using unfold" in new context {
      intStream.zipAll(Stream(1,2)).toList must_== List((Some(1), Some(1)), (Some(2), Some(2)), (Some(3), None))
    }

    "zipAll a Stream with more elements using unfold" in new context {
      intStream.zipAll(Stream(1,2,3,4)).toList must_== List((Some(1), Some(1)), (Some(2), Some(2)), (Some(3), Some(3)), (None, Some(4)))
    }

    "zipAll Streams with no elements using unfold" in new context {
      Stream().zipAll(Stream()) must_== Empty
    }
  }

  "Exercise 5.14" should {
    "return True when one stream startsWith the other" in new context {
      intStream.startsWith(Stream(1,2)) must beTrue
    }

    "return False when one stream doesn't startWith the other" in new context {
      intStream.startsWith(Stream(2)) must beFalse
    }
  }

  trait context extends Scope {
    lazy val intStream = Stream(1,2,3)
  }
}
