package grieks.fpinscala

import org.specs2.mutable.Specification
import org.specs2.specification.Scope
import scala.{Option => _, Some => _, Either => _, Left => _, Right => _, _}

object Chapter4EitherSpec extends Specification {
  import Either._

  "Exercise 4.6" should {
    "map an Either" in new context {
      //Left("exception").map(_ + "should not append") === Left("exception")
      Right(1).map(_.toString) === Right("1")
    }

    "flatMap an Either" in new context {
      Right(1).flatMap(x => if (x>0) Right(x) else Left("exception")) === Right(1)
      Right(0).flatMap(x => if (x>0) Right(x) else Left("exception")) === Left("exception")
    }

    "orElse on Either" in new context {
      Right(1).orElse(Right(0)) === Right(1)
      Left("e").orElse(Right(1)) === Right(1)
      Left("e").orElse(Left("ee")) === Left("ee")
    }

    "map2 on Either" in new context{
      Right(1).map2(Right(2))(_ + _) === Right(3)
      Right(1).map2(Left(1))(_ + _) === Left(1)
    }
  }

  "Exercise 4.7" should {
    "Perform a traverse that maps to Either" in new context {
      traverse(List(0,1))(x => if(x >= 0) Right(x+1) else Left("e")) === Right(List(1,2))
      traverse(List(0,1))(x => if(x < 1) Right(x+1) else Left("e")) === Left("e")
    }

    "Convert a List of Eithers to a single Either" in new context {
      sequence(List(Right(0),Right(1))) === Right(List(0,1))
      sequence(List(Right(0),Left("e"))) === Left("e")
    }
  }

  trait context extends Scope
}
