package grieks.fpinscala

import org.specs2.mutable.Specification
import org.specs2.specification.Scope
import scala.{Option => _, Some => _, Either => _, _}

object Chapter4OptionSpec extends Specification {
  import Option._

  "Exercise 4.1" should {
    "map an Option" in new context {
      None.map(_.toString) === None
      Some(1).map(_.toString) === Some("1")
    }

    "perform getOrElse on an Option" in new context {
      None.getOrElse(1) === 1
      Some("a").getOrElse("b") === "a"
    }

    "perform flatMap on an Option" in new context {
      none[Int].flatMap(x => Some(x+1)) === None
      Some(2).flatMap(x => Some(x-1)) === Some(1)
    }

    "perform orElse on an Option" in new context {
      None.orElse(Some(1)) === Some(1)
      Some(1).orElse(Some(2)) === Some(1)
    }

    "perform filter on an Option" in new context {
      Some(1).filter(_ > 0) === Some(1)
      Some(1).filter(_ > 1) === None
    }
  }

  "Exercise 4.3" should {
    "reduce two Options to a single result" in new context {
      map2(Some(0), Some(1))((a,b) => (a+b).toString) === Some("1")
      map2(none[Int], Some(0))(_ + _) === None
    }
  }

  "Exercise 4.4" should {
    "Convert a List of Options to an Optional List" in new context {
      sequence(List(Some(0), Some(1))) === Some(List(0,1))
      sequence(List(Some(0), None)) === None
    }
  }

  "Exercise 4.5" should {
    "perform traverse that maps to Option" in new context {
      traverse(List(0, 1))(Some(_)) === Some(List(0,1))
      traverse(List(0, 1))(x => if (x > 0) Some(x) else None) === None
    }
  }

  trait context extends Scope {
    def none[A]: Option[A] = None
  }

}
