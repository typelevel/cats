package cats
package arrow

import cats.data.Coproduct

/**
  * `FunctionK[F[_], G[_]]` is a functor transformation from `F` to `G`
  * in the same manner that function `A => B` is a morphism from values
  * of type `A` to `B`.
  * An easy way to create a FunctionK instance is to use the Polymorphic
  * lambdas provided by non/kind-projector v0.9+. E.g.
  * {{{
  *   val listToOption = λ[FunctionK[List, Option]](_.headOption)
  * }}}
  */
trait FunctionK[F[_], G[_]] extends Serializable { self =>

  /**
    * Applies this functor transformation from `F` to `G`
    */
  def apply[A](fa: F[A]): G[A]

  /**
    * Composes two instances of FunctionK into a new FunctionK with this
    * transformation applied last.
    */
  def compose[E[_]](f: FunctionK[E, F]): FunctionK[E, G] =
    new FunctionK[E, G] {
      def apply[A](fa: E[A]): G[A] = self.apply(f(fa))
    }

  /**
    * Composes two instances of FunctionK into a new FunctionK with this
    * transformation applied first.
    */
  def andThen[H[_]](f: FunctionK[G, H]): FunctionK[F, H] =
    f.compose(self)

  /**
    * Composes two instances of FunctionK into a new FunctionK that transforms
    * a [[cats.data.Coproduct]] to a single functor.
    *
    * This transformation will be used to transform left `F` values while
    * `h` will be used to transform right `H` values.
    */
  def or[H[_]](h: FunctionK[H, G]): FunctionK[Coproduct[F, H, ?], G] =
    new FunctionK[Coproduct[F, H, ?], G] {
      def apply[A](fa: Coproduct[F, H, A]): G[A] = fa.fold(self, h)
    }
}

object FunctionK {

  /**
    * The identity transformation of `F` to `F`
    */
  def id[F[_]]: FunctionK[F, F] =
    new FunctionK[F, F] {
      def apply[A](fa: F[A]): F[A] = fa
    }

  /**
    * Lifts function `f` of `F[A] => G[A]` into a `FunctionK[F, G]`.
    *
    * {{{
    *   def headOption[A](list: List[A]): Option[A] = list.headOption
    *   val lifted: FunctionK[List, Option] = FunctionK.lift(headOption)
    * }}}
    *
    * Note: This method has a macro implementation that returns a new
    * `FunctionK` instance as follows:
    *
    * {{{
    *   new FunctionK[F, G] {
    *     def apply[A](fa: F[A]): G[A] = f(fa)
    *   }
    * }}}
    *
    * Additionally, the type parameters on `f` must not be specified.
    */
  def lift[F[_]]: LiftPartiallyApplied[F] = new LiftPartiallyApplied

  private[arrow] final class LiftPartiallyApplied[F[_]](val dummy: Boolean = true) extends AnyVal {
    type T

    def apply[G[_]](f: F[T] => G[T]): FunctionK[F, G] = new FunctionK[F, G] {
      def apply[A](fa: F[A]): G[A] = f(fa.asInstanceOf[F[T]]).asInstanceOf[G[A]]
    }
  }

}
