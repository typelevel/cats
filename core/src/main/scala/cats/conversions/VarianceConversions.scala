package cats
package conversions
import cats.arrow.Profunctor

trait VarianceConversions extends VarianceConversionsLowPriority {
  implicit def autoWidenBifunctor[F[_, _]: Bifunctor, A, B >: A, C, D >: C](fac: F[A, C]): F[B, D] =
    Bifunctor[F].leftWiden(Bifunctor[F].rightFunctor.widen(fac))

  implicit def autoConvertProfunctorVariance[F[_, _]: Profunctor, A, B <: A, C, D >: C](fac: F[A, C]): F[B, D] =
    Profunctor[F].leftNarrow(Profunctor[F].rightWiden(fac))

  implicit def autoNarrowContravariant[F[_]: Contravariant, A, B <: A](fa: F[A]): F[B] = Contravariant[F].narrow(fa)

}

private[cats] trait VarianceConversionsLowPriority {
  implicit def autoWidenFunctor[F[_]: Functor, A, B >: A](fa: F[A]): F[B] = Functor[F].widen(fa)
}
