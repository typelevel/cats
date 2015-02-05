package cats

import simulacrum._

@typeclass trait SemigroupK[F[_]] { self =>
  def combine[A](x: F[A], y: F[A]): F[A]

  def composedWith[G[_]: SemigroupK]: SemigroupK[λ[α => F[G[α]]]] =
    new SemigroupK[λ[α => F[G[α]]]] {
      def combine[A](x: F[G[A]], y: F[G[A]]): F[G[A]] = combine(x, y)
    }

  def algebra[A]: Semigroup[F[A]] =
    new Semigroup[F[A]] {
      def combine(x: F[A], y: F[A]): F[A] = self.combine(x, y)
    }
}
