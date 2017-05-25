package cats
package syntax

trait MonadTransSyntax {
  implicit final def catsSyntaxMonadTrans[F[_], A](fa: F[A]): MonadTransOps[F, A] = new MonadTransOps(fa)
}

final class MonadTransOps[F[_], A](val fa: F[A]) extends AnyVal {
  def liftT[MT[_[_], _]](implicit F: Monad[F], MT: MonadTrans[MT]): MT[F, A] =
    MT.liftT(fa)
}
