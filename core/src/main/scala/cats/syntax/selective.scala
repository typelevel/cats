package cats
package syntax

trait SelectiveSyntax {
  implicit final def catsSyntaxSelective[F[_], A, B](fab: F[Either[A, B]]): SelectiveOps[F, A, B] =
    new SelectiveOps(fab)
  implicit final def catsSyntaxIfS[F[_]](fBool: F[Boolean]): IfSOps[F] =
    new IfSOps(fBool)
  implicit final def catsSyntaxWhenS[F[_]](fBool: F[Boolean]): WhenSOps[F] =
    new WhenSOps(fBool)
}

final class SelectiveOps[F[_], A, B](private val fab: F[Either[A, B]]) extends AnyVal {
  def select(ff: F[A => B])(implicit F: Selective[F]): F[B] = F.select(fab)(ff)
  def branch[C](l: F[A => C])(r: F[B => C])(implicit F: Selective[F]): F[C] = F.branch(fab)(l)(r)
}

final class IfSOps[F[_]](private val fBool: F[Boolean]) extends AnyVal {
  def ifS[A](ifTrue: => F[A], ifFalse: => F[A])(implicit F: Selective[F]): F[A] = F.ifS(fBool)(ifTrue)(ifFalse)
}

final class WhenSOps[F[_]](private val fBool: F[Boolean]) extends AnyVal {
  def whenS[A](fa: F[Unit])(implicit F: Selective[F]): F[Unit] = F.whenS(fBool)(fa)
}
