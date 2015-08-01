package cats
package laws
package discipline

import cats.data.{Cokleisli, Kleisli, NonEmptyList, Validated, Xor, XorT, Ior, Const, OptionT}
import cats.laws.discipline.arbitrary._
import org.scalacheck.Arbitrary

import scala.concurrent.Future

trait ArbitraryK[F[_]] {
  def synthesize[A: Arbitrary]: Arbitrary[F[A]]
}

object ArbitraryK {
  def apply[F[_]](implicit arbk: ArbitraryK[F]): ArbitraryK[F] = arbk

  implicit val id: ArbitraryK[Id] =
    new ArbitraryK[Id] { def synthesize[A: Arbitrary]: Arbitrary[A] = implicitly }

  implicit val nonEmptyList: ArbitraryK[NonEmptyList] =
    new ArbitraryK[NonEmptyList] { def synthesize[A: Arbitrary]: Arbitrary[NonEmptyList[A]] = implicitly }

  implicit val option: ArbitraryK[Option] =
    new ArbitraryK[Option] { def synthesize[A: Arbitrary]: Arbitrary[Option[A]] = implicitly }

  implicit def eitherA[A](implicit A: Arbitrary[A]): ArbitraryK[Either[A, ?]] =
    new ArbitraryK[Either[A, ?]] { def synthesize[B: Arbitrary]: Arbitrary[Either[A, B]] = implicitly }

  implicit def eitherB[B](implicit B: Arbitrary[B]): ArbitraryK[Either[?, B]] =
    new ArbitraryK[Either[?, B]] { def synthesize[A: Arbitrary]: Arbitrary[Either[A, B]] = implicitly }

  implicit def function1A[A]: ArbitraryK[A => ?] =
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

  implicit val eval : ArbitraryK[Eval] =
    new ArbitraryK[Eval] {
      def synthesize[A](implicit A: Arbitrary[A]): Arbitrary[Eval[A]] =
        Arbitrary(A.arbitrary.map(Eval.now(_)))
    }

  implicit def mapA[A](implicit A: Arbitrary[A]): ArbitraryK[Map[A, ?]] =
    new ArbitraryK[Map[A, ?]] { def synthesize[B: Arbitrary]: Arbitrary[Map[A, B]] = implicitly }

  implicit def mapB[B](implicit B: Arbitrary[B]): ArbitraryK[Map[?, B]] =
    new ArbitraryK[Map[?, B]] { def synthesize[A: Arbitrary]: Arbitrary[Map[A, B]] = implicitly }

  implicit def constA[A](implicit A: Arbitrary[A]): ArbitraryK[Const[A, ?]] =
    new ArbitraryK[Const[A, ?]] { def synthesize[B: Arbitrary]: Arbitrary[Const[A, B]] = implicitly }

  implicit def xorA[A](implicit A: Arbitrary[A]): ArbitraryK[A Xor ?] =
    new ArbitraryK[A Xor ?] { def synthesize[B: Arbitrary]: Arbitrary[A Xor B] = implicitly }

  implicit def xorB[B](implicit B: Arbitrary[B]): ArbitraryK[? Xor B] =
    new ArbitraryK[? Xor B] { def synthesize[A: Arbitrary]: Arbitrary[A Xor B] = implicitly }

  implicit def xorTA[F[_], A](implicit F: ArbitraryK[F], A: Arbitrary[A]): ArbitraryK[XorT[F, A, ?]] =
    new ArbitraryK[XorT[F, A, ?]] { def synthesize[B: Arbitrary]: Arbitrary[XorT[F, A, B]] = implicitly }

  implicit def xorTB[F[_], B](implicit F: ArbitraryK[F], B: Arbitrary[B]): ArbitraryK[XorT[F, ?, B]] =
    new ArbitraryK[XorT[F, ?, B]] { def synthesize[A: Arbitrary]: Arbitrary[XorT[F, A, B]] = implicitly }

  implicit def validA[A](implicit A: Arbitrary[A]): ArbitraryK[Validated[A, ?]] =
    new ArbitraryK[Validated[A, ?]] { def synthesize[B: Arbitrary]: Arbitrary[Validated[A, B]] = implicitly }

  implicit def validB[B](implicit B: Arbitrary[B]): ArbitraryK[Validated[?, B]] =
    new ArbitraryK[Validated[?, B]] { def synthesize[A: Arbitrary]: Arbitrary[Validated[A, B]] = implicitly }

  implicit def iorA[A](implicit A: Arbitrary[A]): ArbitraryK[A Ior ?] =
    new ArbitraryK[A Ior ?] { def synthesize[B: Arbitrary]: Arbitrary[A Ior B] = implicitly }

  implicit def iorB[B](implicit B: Arbitrary[B]): ArbitraryK[? Ior B] =
    new ArbitraryK[? Ior B] { def synthesize[A: Arbitrary]: Arbitrary[A Ior B] = implicitly }

  implicit def kleisliA[F[_], A](implicit F: ArbitraryK[F]): ArbitraryK[Kleisli[F, A, ?]] =
    new ArbitraryK[Kleisli[F, A, ?]]{ def synthesize[B: Arbitrary]: Arbitrary[Kleisli[F, A, B]] = implicitly }

  implicit def cokleisliA[F[_], A]: ArbitraryK[Cokleisli[F, A, ?]] =
    new ArbitraryK[Cokleisli[F, A, ?]]{ def synthesize[B: Arbitrary]: Arbitrary[Cokleisli[F, A, B]] = implicitly }

  implicit def futureArbitraryK: ArbitraryK[Future] =
    new ArbitraryK[Future] {
      def synthesize[A](implicit A: Arbitrary[A]): Arbitrary[Future[A]] =
        Arbitrary(A.arbitrary.map(Future.successful))
    }

  implicit val set: ArbitraryK[Set] =
    new ArbitraryK[Set] { def synthesize[A: Arbitrary]: Arbitrary[Set[A]] = implicitly }

  implicit val stream: ArbitraryK[Stream] =
    new ArbitraryK[Stream] { def synthesize[A: Arbitrary]: Arbitrary[Stream[A]] = implicitly }

  implicit val vector: ArbitraryK[Vector] =
    new ArbitraryK[Vector] { def synthesize[A: Arbitrary]: Arbitrary[Vector[A]] = implicitly }

  implicit def optionT[F[_]](implicit F: ArbitraryK[F]): ArbitraryK[OptionT[F, ?]] =
    new ArbitraryK[OptionT[F, ?]] { def synthesize[A: Arbitrary]: Arbitrary[OptionT[F, A]] = implicitly }
}
