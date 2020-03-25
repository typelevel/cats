package cats
package arrow

import simulacrum.typeclass

/**
 * A [[Profunctor]] is a [[Contravariant]] functor on its first type parameter
 * and a [[Functor]] on its second type parameter.
 *
 * Must obey the laws defined in cats.laws.ProfunctorLaws.
 */
@typeclass trait Profunctor[F[_, _]] { self =>

  /**
   * Contramap on the first type parameter and map on the second type parameter
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> import cats.arrow.Profunctor
   * scala> val fab: Double => Double = x => x + 0.3
   * scala> val f: Int => Double = x => x.toDouble / 2
   * scala> val g: Double => Double = x => x * 3
   * scala> val h = Profunctor[Function1].dimap(fab)(f)(g)
   * scala> h(3)
   * res0: Double = 5.4
   * }}}
   */
  def dimap[A, B, C, D](fab: F[A, B])(f: C => A)(g: B => D): F[C, D]

  /**
   * contramap on the first type parameter
   */
  def lmap[A, B, C](fab: F[A, B])(f: C => A): F[C, B] =
    dimap(fab)(f)(identity)

  /**
   * map on the second type parameter
   */
  def rmap[A, B, C](fab: F[A, B])(f: B => C): F[A, C] =
    dimap[A, B, A, C](fab)(identity)(f)
}

object Profunctor {
  implicit def catsStrongForFunction1: Strong[Function1] =
    cats.instances.function.catsStdInstancesForFunction1
}
