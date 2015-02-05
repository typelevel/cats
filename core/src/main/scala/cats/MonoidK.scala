package cats

import simulacrum._

@typeclass trait MonoidK[F[_]] extends SemigroupK[F] { self =>
  def empty[A]: F[A]

  override def algebra[A]: Monoid[F[A]] =
    new Monoid[F[A]] {
      def empty: F[A] = self.empty
      def combine(x: F[A], y: F[A]): F[A] = self.combine(x, y)
    }
}

// object MonoidK {
//   def apply[F[_]](implicit ev: MonoidK[F]): MonoidK[F] = ev
// }
