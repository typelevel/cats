package cats.laws.discipline

import cats.data._
import org.scalacheck.{Arbitrary, Gen}

/**
 * Arbitrary instances for cats.data
 */
object arbitrary {

  implicit def constArbitrary[A, B](implicit A: Arbitrary[A]): Arbitrary[Const[A, B]] =
    Arbitrary(A.arbitrary.map(Const[A, B]))

  implicit def oneAndArbitrary[F[_], A](implicit A: Arbitrary[A], F: ArbitraryK[F]): Arbitrary[OneAnd[A, F]] =
    Arbitrary(F.synthesize[A].arbitrary.flatMap(fa => A.arbitrary.map(a => OneAnd(a, fa))))

  implicit def xorArbitrary[A, B](implicit A: Arbitrary[A], B: Arbitrary[B]): Arbitrary[A Xor B] =
    Arbitrary(Gen.oneOf(A.arbitrary.map(Xor.left), B.arbitrary.map(Xor.right)))

  implicit def validatedArbitrary[A, B](implicit A: Arbitrary[A], B: Arbitrary[B]): Arbitrary[Validated[A, B]] =
    Arbitrary(Gen.oneOf(A.arbitrary.map(Validated.invalid), B.arbitrary.map(Validated.valid)))

  implicit def kleisliArbitrary[F[_], A, B](implicit F: ArbitraryK[F], B: Arbitrary[B]): Arbitrary[Kleisli[F, A, B]] =
    Arbitrary(F.synthesize[B].arbitrary.map(fb => Kleisli[F, A, B](_ => fb)))

  implicit def cokleisliArbitrary[F[_], A, B](implicit B: Arbitrary[B]): Arbitrary[Cokleisli[F, A, B]] =
    Arbitrary(B.arbitrary.map(b => Cokleisli[F, A, B](_ => b)))
}
