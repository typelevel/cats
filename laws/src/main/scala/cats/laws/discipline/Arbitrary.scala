package cats.laws.discipline

import cats.data._
import org.scalacheck.{Arbitrary, Gen}

/**
 * Arbitrary instances for cats.data
 */
object arbitrary {

  implicit def constArbitrary[A, B](implicit A: Arbitrary[A]): Arbitrary[Const[A, B]] =
    Arbitrary(A.arbitrary.map(Const[A, B]))

  implicit def orArbitrary[A, B](implicit A: Arbitrary[A], B: Arbitrary[B]): Arbitrary[A Or B] =
    Arbitrary(Gen.oneOf(A.arbitrary.map(Or.left), B.arbitrary.map(Or.right)))

  implicit def kleisliArbitrary[F[_], A, B](implicit F: ArbitraryK[F], B: Arbitrary[B]): Arbitrary[Kleisli[F, A, B]] =
    Arbitrary(F.synthesize[B].arbitrary.map(fb => Kleisli[F, A, B](_ => fb)))

  implicit def cokleisliArbitrary[F[_], A, B](implicit B: Arbitrary[B]): Arbitrary[Cokleisli[F, A, B]] =
    Arbitrary(B.arbitrary.map(b => Cokleisli[F, A, B](_ => b)))

  implicit def iListArbitrary[A](implicit A: Arbitrary[A]): Arbitrary[IList[A]] =
    Arbitrary(Gen.listOf(A.arbitrary).map(as => IList(as: _*)))
}
