package cats
package arrow

import cats.data.{Xor, Coproduct}

trait NaturalTransformation[F[_], G[_]] extends Serializable { self =>
  def apply[A](fa: F[A]): G[A]

  def compose[E[_]](f: NaturalTransformation[E, F]): NaturalTransformation[E, G] =
    new NaturalTransformation[E, G] {
      def apply[A](fa: E[A]): G[A] = self.apply(f(fa))
    }

  def andThen[H[_]](f: NaturalTransformation[G, H]): NaturalTransformation[F, H] =
    f.compose(self)
}

object NaturalTransformation {
  def id[F[_]]: NaturalTransformation[F, F] =
    new NaturalTransformation[F, F] {
      def apply[A](fa: F[A]): F[A] = fa
    }

  def or[F[_], G[_], H[_]](f: F ~> H, g: G ~> H): Coproduct[F, G, ?] ~> H =
    new (Coproduct[F, G, ?] ~> H) {
      def apply[A](fa: Coproduct[F, G, A]): H[A] = fa.run match {
        case Xor.Left(ff) => f(ff)
        case Xor.Right(gg) => g(gg)
      }
    }
}
