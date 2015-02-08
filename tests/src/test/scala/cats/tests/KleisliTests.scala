package cats.tests

import cats.Functor
import cats.data.Kleisli
import org.scalatest.FunSuite


class KleisliTests extends FunSuite {

  /** MyId id is a dummy case class used to test implicit resolution */
  case class MyId[A](runId: A)

  test("Kleisli is a Functor if F is a Functor") {
    implicit val myIdFunctor: Functor[MyId] = new Functor[MyId]{
      def map[A, B](fa: MyId[A])(f: A => B): MyId[B] = MyId(f(fa.runId))
    }

    val plus1Times2: Kleisli[MyId, Int, Int] = Functor[Kleisli[MyId, Int, ?]].map(Kleisli((i: Int) => MyId(1 + i)))(_ * 2)
    assert(plus1Times2.runKleisli(1) == MyId(4))
  }
}
