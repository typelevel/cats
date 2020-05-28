package cats

trait AutoVariance extends AutoVarianceLowPriority {
  implicit def autoWidenBifunctor[F[_, _]: Bifunctor, A, B >: A, C, D >: C](fac: F[A, C]): F[B, D] =
    Bifunctor[F].leftWiden(Bifunctor[F].rightFunctor.widen(fac))

  implicit def autoNarrowContravariant[F[_]: Contravariant, A, B <: A](fa: F[A]): F[B] = Contravariant[F].narrow(fa)

}

trait AutoVarianceLowPriority {
  implicit def autoWidenFunctor[F[_]: Functor, A, B >: A](fa: F[A]): F[B] = Functor[F].widen(fa)
}
