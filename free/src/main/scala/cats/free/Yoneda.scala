package cats
package free

/**
 * The cofree functor for `F`. The Yoneda lemma says that
 * `Yoneda[F,A]` is isomorphic to `F[A]` for any functor `F`.
 * The function from `Yoneda[F, A]` to `F[A]` exists even when
 * we have forgotten that `F` is a functor.
 * Can be seen as a partially applied `map` for the functor `F`.
 */
abstract class Yoneda[F[_], A] extends Serializable { self =>
  def apply[B](f: A => B): F[B]

  /**
   * Converts to `F[A]` even without a `Functor` instance for `F`.
   */
  def run: F[A] = apply(a => a)

  /**
   * Converts to `Coyoneda[F,A]` even without a `Functor` instance for `F`.
   */
  def toCoyoneda: Coyoneda.Aux[F, A, A] = Coyoneda(run)(identity[A])

  /**
   * Simple function composition. Allows map fusion without traversing an `F`.
   */
  def map[B](f: A => B): Yoneda[F, B] =
    new Yoneda[F, B] {
      def apply[C](g: B => C): F[C] = self(f.andThen(g))
    }

  /**
   * Modify the context `F` using transformation `f`.
   */
  def mapK[G[_]](f: F ~> G): Yoneda[G, A] =
    new Yoneda[G, A] {
      def apply[B](g: A => B): G[B] = f(self(g))
    }
}

object Yoneda {

  /**
   * `Yoneda[F, _]` is a functor for any `F`.
   */
  implicit def catsFreeFunctorForYoneda[F[_]]: Functor[Yoneda[F, *]] =
    new Functor[Yoneda[F, *]] {
      def map[A, B](ya: Yoneda[F, A])(f: A => B): Yoneda[F, B] = ya.map(f)
    }

  /**
   * `F[A]` converts to `Yoneda[F, A]` for any functor `F`.
   */
  def apply[F[_], A](fa: F[A])(implicit F: Functor[F]): Yoneda[F, A] =
    new Yoneda[F, A] {
      def apply[B](f: A => B): F[B] = F.map(fa)(f)
    }
}
