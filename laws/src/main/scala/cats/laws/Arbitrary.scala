package cats.laws

import cats.data.{Or, Const}
import org.scalacheck.{Gen, Arbitrary}

/**
 * Arbitrary instances for cats.data
 */
object arbitrary {

  implicit def constArbitrary[A, B](implicit A: Arbitrary[A]): Arbitrary[Const[A, B]] =
    Arbitrary(A.arbitrary.map(Const[A, B](_)))

  implicit def orArbitrary[A, B](implicit A: Arbitrary[A], B: Arbitrary[B]): Arbitrary[A Or B] =
    Arbitrary(Gen.oneOf(A.arbitrary.map(Or.left), B.arbitrary.map(Or.right)))
}
