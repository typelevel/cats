package cats
package syntax

trait Function1Syntax {

  implicit def catsSyntaxFunction1[F[_]: Functor, A, B](fab: F[Function1[A, B]]): Function1Ops[F, A, B] =
    new Function1Ops[F, A, B](fab)

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
}
