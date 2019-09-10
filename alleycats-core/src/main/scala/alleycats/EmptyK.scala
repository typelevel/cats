package alleycats

import simulacrum.typeclass

@typeclass trait EmptyK[F[_]] { self =>
  def empty[A]: F[A]

  def synthesize[A]: Empty[F[A]] =
    new Empty[F[A]] {
      def empty: F[A] = self.empty[A]
    }
}
