package cats
package syntax

trait RepresentableSyntax {
  implicit final def catsSyntaxTabulate[A, R](f: R => A): TabulateOps[A, R] =
    new TabulateOps[A, R](f)

  implicit final def catsSyntaxIndex[F[_], A](fa: F[A]): IndexOps[F, A] =
    new IndexOps[F, A](fa)
}

final class IndexOps[F[_], A](private val fa: F[A]) extends AnyVal {
  def index[R](implicit R: Representable.Aux[F, R]): R => A = R.index(fa)
}

final class TabulateOps[A, R](private val f: R => A) extends AnyVal {
  def tabulate[F[_]](implicit R: Representable.Aux[F, R]): F[A] = R.tabulate(f)
}
