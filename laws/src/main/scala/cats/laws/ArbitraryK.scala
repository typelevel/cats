package cats.laws

import cats.data.Const
import org.scalacheck.Arbitrary
import cats.laws.arbitrary._

trait ArbitraryK[F[_]] {
  def synthesize[A: Arbitrary]: Arbitrary[F[A]]
}

object ArbitraryK {
  implicit val option: ArbitraryK[Option] =
    new ArbitraryK[Option] { def synthesize[A: Arbitrary]: Arbitrary[Option[A]] = implicitly }

  implicit def f1a[A: Arbitrary]: ArbitraryK[A => ?] =
    new ArbitraryK[A => ?] { def synthesize[B: Arbitrary]: Arbitrary[A => B] = implicitly }

  implicit def f1b[B: Arbitrary]: ArbitraryK[? => B] =
    new ArbitraryK[? => B] { def synthesize[A: Arbitrary]: Arbitrary[A => B] = implicitly }

  implicit val list: ArbitraryK[List] =
    new ArbitraryK[List] { def synthesize[A: Arbitrary]: Arbitrary[List[A]] = implicitly }

  implicit def constArbitraryK[A](implicit A: Arbitrary[A]): ArbitraryK[Const[A, ?]] =
    new ArbitraryK[Const[A, ?]] { def synthesize[B: Arbitrary]: Arbitrary[Const[A, B]] = implicitly }
}
