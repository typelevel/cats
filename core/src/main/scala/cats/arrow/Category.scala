package cats
package arrow

/**
 * Must obey the laws defined in cats.laws.CategoryLaws.
 */
trait Category[F[_, _]] extends Compose[F] { self =>

  def id[A]: F[A, A]

  override def algebraK: MonoidK[λ[α => F[α, α]]] =
    new MonoidK[λ[α => F[α, α]]] {
      def empty[A]: F[A, A] = id
      def combineK[A](f1: F[A, A], f2: F[A, A]): F[A, A] = self.compose(f1, f2)
    }

  override def algebra[A]: Monoid[F[A, A]] =
    new Monoid[F[A, A]] {
      def empty: F[A, A] = id
      def combine(f1: F[A, A], f2: F[A, A]): F[A, A] = self.compose(f1, f2)
    }
}

object Category {
  def apply[F[_, _]](implicit ev: Category[F]): Category[F] = ev
}
