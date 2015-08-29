package cats
package arrow

/**
 * Must obey the laws defined in cats.laws.ComposeLaws.
 */
trait Compose[F[_, _]] extends Serializable { self =>
  def compose[A, B, C](f: F[B, C], g: F[A, B]): F[A, C]

  def andThen[A, B, C](f: F[A, B], g: F[B, C]): F[A, C] =
    compose(g, f)

  def algebraK: SemigroupK[λ[α => F[α, α]]] =
    new SemigroupK[λ[α => F[α, α]]] {
      def combine[A](f1: F[A, A], f2: F[A, A]): F[A, A] = self.compose(f1, f2)
    }

  def algebra[A]: Semigroup[F[A, A]] =
    new Semigroup[F[A, A]] {
      def combine(f1: F[A, A], f2: F[A, A]): F[A, A] = self.compose(f1, f2)
    }
}

object Compose {
  def apply[F[_, _]](implicit ev: Compose[F]): Compose[F] = ev
}
