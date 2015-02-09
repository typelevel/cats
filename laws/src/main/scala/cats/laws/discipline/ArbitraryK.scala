package cats.laws.discipline

import cats.data.{Or, Const}
import org.scalacheck.Arbitrary
import cats.laws.discipline.arbitrary._

trait ArbitraryK[F[_]] {
  def synthesize[A: Arbitrary]: Arbitrary[F[A]]
}

object ArbitraryK {
  implicit val option: ArbitraryK[Option] =
    new ArbitraryK[Option] { def synthesize[A: Arbitrary]: Arbitrary[Option[A]] = implicitly }

  implicit def function1A[A: Arbitrary]: ArbitraryK[A => ?] =
    new ArbitraryK[A => ?] { def synthesize[B: Arbitrary]: Arbitrary[A => B] = implicitly }

  implicit def function1B[B: Arbitrary]: ArbitraryK[? => B] =
    new ArbitraryK[? => B] { def synthesize[A: Arbitrary]: Arbitrary[A => B] = implicitly }

  implicit val function0: ArbitraryK[Function0] =
    new ArbitraryK[Function0] {
      def synthesize[A](implicit A: Arbitrary[A]): Arbitrary[() => A] =
        Arbitrary(A.arbitrary.map(a => () => a))
    }

  implicit val list: ArbitraryK[List] =
    new ArbitraryK[List] { def synthesize[A: Arbitrary]: Arbitrary[List[A]] = implicitly }

  implicit def constA[A](implicit A: Arbitrary[A]): ArbitraryK[Const[A, ?]] =
    new ArbitraryK[Const[A, ?]] { def synthesize[B: Arbitrary]: Arbitrary[Const[A, B]] = implicitly }

  implicit def orA[A](implicit A: Arbitrary[A]): ArbitraryK[A Or ?] =
    new ArbitraryK[A Or ?] { def synthesize[B: Arbitrary]: Arbitrary[A Or B] = implicitly }

  implicit def orB[B](implicit B: Arbitrary[B]): ArbitraryK[? Or B] =
    new ArbitraryK[? Or B] { def synthesize[A: Arbitrary]: Arbitrary[A Or B] = implicitly }
}
