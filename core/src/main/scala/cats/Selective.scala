package cats

import simulacrum.{typeclass}
import scala.annotation.implicitNotFound

@implicitNotFound("Could not find an instance of Selective for ${F}")
@typeclass trait Selective[F[_]] extends Applicative[F] {
  def select[A, B](fab: F[Either[A, B]])(ff: F[A => B]): F[B]
}

object Selective {
  def fromApplicative[F[_], A, B](fab: F[Either[A, B]])(ff: F[A => B])(implicit F: Applicative[F]): F[B] =
    F.map2(fab, ff) {
      case (Left(a), f)  => f(a)
      case (Right(b), f) => b
    }
}
