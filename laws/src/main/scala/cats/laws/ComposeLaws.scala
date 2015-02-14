package cats.laws

import cats.arrow.Compose
import cats.syntax.compose._

trait ComposeLaws[F[_, _]] {
  implicit def F: Compose[F]

  def composeAssociativity[A, B, C, D](fab: F[A, B], fbc: F[B, C], fcd: F[C, D]): (F[A, D], F[A, D]) =
    ((fab andThen fbc) andThen fcd) -> (fab andThen (fbc andThen fcd))
}

object ComposeLaws {
  def apply[F[_, _]](implicit ev: Compose[F]): ComposeLaws[F] =
    new ComposeLaws[F] { def F = ev }
}
