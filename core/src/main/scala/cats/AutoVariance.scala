package cats

trait AutoVariance {
  implicit def autoWidenFunctor[F[_]: Functor, A, B >: A](fa: F[A]): F[B] = Functor[F].widen(fa)

  implicit def autoNarrowContravariant[F[_]: Contravariant, A, B <: A](fa: F[A]): F[B] = Contravariant[F].narrow(fa)

  implicit def autoLeftWidenBifunctor[F[_, _]: Bifunctor, A, B >: A, C](fac: F[A, C]): F[B, C] =
    Bifunctor[F].leftWiden(fac)
}
