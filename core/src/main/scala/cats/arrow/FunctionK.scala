package cats
package arrow

import cats.data.{Coproduct, Xor}

trait FunctionK[F[_], G[_]] extends Serializable { self =>
  def apply[A](fa: F[A]): G[A]

  def compose[E[_]](f: FunctionK[E, F]): FunctionK[E, G] =
    new FunctionK[E, G] {
      def apply[A](fa: E[A]): G[A] = self.apply(f(fa))
    }

  def andThen[H[_]](f: FunctionK[G, H]): FunctionK[F, H] =
    f.compose(self)

  def or[H[_]](h: FunctionK[H, G]): FunctionK[Coproduct[F, H, ?], G] =
    new FunctionK[Coproduct[F, H, ?], G] {
      def apply[A](fa: Coproduct[F, H, A]): G[A] = fa.run match {
        case Xor.Left(ff) => self(ff)
        case Xor.Right(gg) => h(gg)
      }
    }
}

object FunctionK {
  def id[F[_]]: FunctionK[F, F] =
    new FunctionK[F, F] {
      def apply[A](fa: F[A]): F[A] = fa
    }
}
