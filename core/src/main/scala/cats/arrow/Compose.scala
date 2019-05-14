package cats
package arrow

import simulacrum.typeclass

/**
 * Must obey the laws defined in cats.laws.ComposeLaws.
 *
 * Here's how you can use `>>>` and `<<<`
 * Example:
 * {{{
 * scala> import cats.implicits._
 * scala> val f : Int => Int = (_ + 1)
 * scala> val g : Int => Int = (_ * 100)
 * scala> (f >>> g)(3)
 * res0: Int = 400
 * scala> (f <<< g)(3)
 * res1: Int = 301
 * }}}
 */
@typeclass trait Compose[ |==>[_, _]] { self =>

  @simulacrum.op("<<<", alias = true)
  def compose[A, B, C](f: B |==> C, g: A |==> B): A |==> C

  @simulacrum.op(">>>", alias = true)
  def andThen[A, B, C](f: A |==> B, g: B |==> C): A |==> C =
    compose(g, f)

  def algebraK: SemigroupK[λ[α => α |==> α]] =
    new SemigroupK[λ[α => α |==> α]] {
      def combineK[A](f1: A |==> A, f2: A |==> A): A |==> A = self.compose(f1, f2)
    }

  def algebra[A]: Semigroup[A |==> A] =
    new Semigroup[A |==> A] {
      def combine(f1: A |==> A, f2: A |==> A): A |==> A = self.compose(f1, f2)
    }
}
