package cats.tests

import cats.Copair
import cats.data.{Validated, Xor}
import cats.laws.discipline.CopairTests
import cats.laws.discipline.arbitrary._
import org.scalacheck.Arbitrary

class CopairTest extends CatsSuite {

  checkAll("Xor", CopairTests[Xor].copair[Option, Int, Int, Int, String, String, String])

  testCopairs[Xor]("Xor")
  testCopairs[Either]("Either")
  testCopairs[Validated]("Validated")

  def testCopairs[F[_,_]: Copair](ofType: String)(implicit arb: Arbitrary[F[String, Int]]): Unit = {

    test(s"$ofType Copair for-each performs side-effect") {

      forAll { copair: F[String, Int] =>
        var sideEffectOccurred = false
        copair.foreach(_ => sideEffectOccurred = true)

        sideEffectOccurred should === (copair.isRight)
      }
    }

    test(s"$ofType Copair for-all") {
      forAll { copair: F[String, Int] =>
        copair.forall(_ % 2 == 0) should === (copair.fold(_ => true, _ % 2 == 0))
      }
    }

    test(s"$ofType Copair exists") {
      forAll { copair: F[String, Int] =>
        copair.exists(_ % 2 == 0) should === (copair.fold(_ => false, _ % 2 == 0))
      }
    }

    test(s"$ofType Copair left/right") {
      forAll { copair: F[String, Int] =>
        copair.isLeft should === (copair.fold(_ => true, _ => false))
        copair.isRight should === (copair.fold(_ => false, _ => true))
      }
    }


  }
}
