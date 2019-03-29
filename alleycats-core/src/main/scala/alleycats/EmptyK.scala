package alleycats

import export._
import simulacrum.typeclass

@typeclass trait EmptyK[F[_]] { self =>
  def empty[A]: F[A]

  def synthesize[A]: Empty[F[A]] =
    new Empty[F[A]] {
      def empty: F[A] = self.empty[A]
    }
}

@imports[EmptyK]
object EmptyK

@exports
object EmptyKInstances {
  @export(Instantiated)
  implicit def instantiate[F[_], T](implicit ekf: EmptyK[F]): Empty[F[T]] = ekf.synthesize[T]
}
