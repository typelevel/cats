package cats.tests

import cats.Copair
import cats.data.{Validated, Xor}
import cats.laws.discipline.{CopairTests => CopairLaws}
import cats.laws.discipline.arbitrary._
import org.scalacheck.Arbitrary

class CopairTests extends CatsSuite {

  checkAll("Xor", CopairLaws[Xor].copair[Option, Int, Int, Int, String, String, String])

  testCopairs[Xor]("Xor")
  testCopairs[Either]("Either")
  testCopairs[Validated]("Validated")

  def testCopairs[F[_,_]: Copair](ofType: String)(implicit arb: Arbitrary[F[String, Int]]): Unit = {

    test(s"$ofType Copair for-each performs side-effect") {

      forAll { copair: F[String, Int] =>

        def copairForeach[A, B](f: F[A,B])(fb: B => Unit): Unit = f.foreach(fb)

        var sideEffectOccurred = false
        copairForeach(copair)(_ => sideEffectOccurred = true)

        sideEffectOccurred should === (copair.isRight)
      }
    }

    /**
      * The tests defined below have a def that tests the [[Copair]] implementation,
      * followed by a comparison to the [[Xor]] implementation.
      *
      * They should follow this template:
      *   def copairFoo[A, B](f: F[A,B])...
      *   copairFoo(copair) should === (copair.foo)
      */

    test(s"$ofType Copair for-all") {
      forAll { copair: F[String, Int] =>

        def copairForAll[A, B](f: F[A, B])(fb: B => Boolean): Boolean = f.forall(fb)

        copairForAll(copair)(_ % 2 == 0) should ===(copair.forall(_ % 2 == 0))
      }
    }

    test(s"$ofType Copair exists") {
      forAll { copair: F[String, Int] =>

        def copairExists[A, B](f: F[A, B])(fb: B => Boolean): Boolean = f.exists(fb)

        copairExists(copair)(_ % 2 == 0) should ===(copair.exists(_ % 2 == 0))
      }
    }

    test(s"$ofType Copair left/right") {
      forAll { copair: F[String, Int] =>

        def copairIsLeft(f: F[_, _]): Boolean = f.isLeft
        def copairIsRight(f: F[_, _]): Boolean = f.isRight

        copairIsLeft(copair) should ===(copair.isLeft)
        copairIsRight(copair) should ===(copair.isRight)
      }
    }


  }
}
