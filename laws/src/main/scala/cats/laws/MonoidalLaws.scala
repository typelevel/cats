package cats.laws

import cats.Monoidal

trait MonoidalLaws[F[_]] {

  implicit def F: Monoidal[F]

}

object MonoidalLaws {

  def apply[F[_]](implicit ev: Monoidal[F]): MonoidalLaws[F] =
    new MonoidalLaws[F] { def F: Monoidal[F] = ev }

}