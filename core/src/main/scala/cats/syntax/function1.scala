package cats
package syntax

import cats.arrow.Compose
import cats.data.Kleisli

trait Function1Syntax {

  implicit def catsSyntaxFunction1[F[_]: Functor, A, B](fab: F[Function1[A, B]]): Function1Ops[F, A, B] =
    new Function1Ops[F, A, B](fab)

  implicit def catsSyntaxFunction1ComposeKleisli[F[_], A, B](
    fab: Function1[A, F[B]]
  )(implicit ev: Compose[Kleisli[F, *, *]]): Function1ComposeKleisliOps[F, A, B] =
    new Function1ComposeKleisliOps[F, A, B](fab)

  final class Function1Ops[F[_]: Functor, A, B](fab: F[Function1[A, B]]) {

    /**
     * Given a function in the Functor context and a plain value, supplies the
     * value to the function.
     *
     * Example:
     * {{{
     * scala> import cats.implicits._
     *
     * scala> val someF: Option[Int => Long] = Some(_.toLong + 1L)
     * scala> val noneF: Option[Int => Long] = None
     * scala> val anInt: Int = 3
     *
     * scala> someF.mapApply(anInt)
     * res0: Option[Long] = Some(4)
     *
     * scala> noneF.mapApply(anInt)
     * res1: Option[Long] = None
     *
     * }}}
     */
    def mapApply(a: A): F[B] = Functor[F].map(fab)(_(a))
  }

  final class Function1ComposeKleisliOps[F[_], A, B](f: A => F[B])(implicit CK: Compose[Kleisli[F, *, *]]) {

    /**
     * Alias for `(Kleisli(f) >>> Kleisli(g)).run` or `(Kleisli(f) andThen Kleisli(g)).run`
     */
    def >=>[C](g: B => F[C]): A => F[C] = CK.andThen(Kleisli(f), Kleisli(g)).run

    /**
     * Alias for `(Kleisli(f) <<< Kleisli(g)).run` or `(Kleisli(f) compose Kleisli(g)).run`
     */
    def <=<[C](g: C => F[A]): C => F[B] = CK.compose(Kleisli(f), Kleisli(g)).run
  }
}
