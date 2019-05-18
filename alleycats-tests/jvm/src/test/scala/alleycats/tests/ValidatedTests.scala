package alleycats.tests

import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import org.scalatest.Matchers
import org.scalatest.flatspec.AnyFlatSpec

class ValidatedTests extends AnyFlatSpec with Matchers {

  "import alleycats.data.validated.FlatMap._" should "allow flatmap on Validated" in {
    import alleycats.data.validated.FlatMap._
    val foobar = for {
      foo <- Validated.valid[Int, String]("foo")
      bar <- Validated.valid[Int, String]("bar")
    } yield {
      foo + bar
    }

    foobar should be(Valid("foobar"))

    val notfoobar1 = for {
      notfoo <- Validated.invalid[Int, String](1)
      notbar <- Validated.invalid[Int, String](2)
    } yield {
      notfoo + notbar
    }

    notfoobar1 should be(Invalid(1))

    val notfoobar2 = for {
      notfoo <- Validated.invalid[Int, String](1)
      bar <- Validated.valid[Int, String]("bar")
    } yield {
      notfoo + bar
    }

    notfoobar2 should be(Invalid(1))

    val notfoobar3 = for {
      foo <- Validated.valid[Int, String]("foo")
      notbar <- Validated.invalid[Int, String](2)
    } yield {
      foo + notbar
    }

    notfoobar3 should be(Invalid(2))
  }

}
