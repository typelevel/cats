package cats
package syntax

trait RepresentableSyntax {
  implicit final def catsSyntaxTabulate[A, R](f: R => A): TabulateOps[A, R] =
    new TabulateOps[A, R](f)

  implicit final def catsSyntaxIndex[F[_], A, R](fa: F[A])(implicit R: Representable.Aux[F, R]): IndexOps[F, A, R] =
    new IndexOps[F, A, R](fa)
}

final class IndexOps[F[_], A, R](val fa: F[A]) extends AnyVal {
  def index(implicit R: Representable.Aux[F, R]): R => A = R.index(fa)
}

final class TabulateOps[A, R](val f: R => A) extends AnyVal {
  def tabulate[F[_]](implicit R: Representable.Aux[F, R]): F[A] = R.tabulate(f)
}
