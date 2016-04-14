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

  test("Copair for-all") {
    forAll { copair: Xor[String, Int] =>

      def copairForAll[F[_,_]: Copair, A, B](f: F[A,B])(fb: B => Boolean): Boolean = f.forall(fb)

      copairForAll(copair)(_ % 2 == 0) should === (copair.fold(_ => true, _ % 2 == 0))
    }
  }

  test("Copair exists") {
    forAll { copair: Xor[String, Int] =>

      def copairExists[F[_,_]: Copair, A, B](f: F[A,B])(fb: B => Boolean): Boolean = f.exists(fb)

      copairExists(copair)(_ % 2 == 0) should === (copair.fold(_ => false, _ % 2 == 0))
    }
  }

  test("Copair left/right") {
    forAll { copair: Xor[String, Int] =>

      def copairIsLeft[F[_,_]: Copair](f: F[_,_]): Boolean = f.isLeft
      def copairIsRight[F[_,_]: Copair](f: F[_,_]): Boolean = f.isRight

      copairIsLeft(copair) should === (copair.fold(_ => true, _ => false))
      copairIsRight(copair) should === (copair.fold(_ => false, _ => true))
    }
  }
}
