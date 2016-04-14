package cats.tests

import cats.Copair
import cats.data.Xor
import cats.laws.discipline.{CopairTests => CopairLaws}
import cats.laws.discipline.arbitrary._

class CopairTests extends CatsSuite {

  checkAll(" ", CopairLaws[Xor].copair[Option, Int, Int, Int, String, String, String])

  test("Copair for-each performs side-effect") {

    forAll { copair: Xor[Int, String] =>

      def copairForeach[F[_,_]: Copair, A, B](f: F[A,B])(fb: B => Unit): Unit = f.foreach(fb)

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
    *   def copairFoo[F[_,_]: Copair, A, B](f: F[A,B])...
    *   copairFoo(copair) should === (copair.foo)
    */

  test("Copair for-all") {
    forAll { copair: Xor[String, Int] =>

      def copairForAll[F[_,_]: Copair, A, B](f: F[A,B])(fb: B => Boolean): Boolean = f.forall(fb)

      copairForAll(copair)(_ % 2 == 0) should === (copair.forall(_ % 2 == 0))
    }
  }

  test("Copair exists") {
    forAll { copair: Xor[String, Int] =>

      def copairExists[F[_,_]: Copair, A, B](f: F[A,B])(fb: B => Boolean): Boolean = f.exists(fb)

      copairExists(copair)(_ % 2 == 0) should === (copair.exists(_ % 2 == 0))
    }
  }

  test("Copair left/right") {
    forAll { copair: Xor[String, Int] =>

      def copairIsLeft[F[_,_]: Copair](f: F[_,_]): Boolean = f.isLeft
      def copairIsRight[F[_,_]: Copair](f: F[_,_]): Boolean = f.isRight

      copairIsLeft(copair) should === (copair.isLeft)
      copairIsRight(copair) should === (copair.isRight)
    }
  }
}
