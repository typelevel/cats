package cats
package tests

import cats.laws.discipline.ExhaustiveCheck

import org.scalacheck.{Arbitrary, Cogen}, Arbitrary.arbitrary

/**
 * Similar to [[ListWrapper]], but using `Option` instead of `List` limits the size of the structure, which can be
 * useful for limiting the space of test values to generate.
 */
final case class OptionWrapper[A](option: Option[A]) extends AnyVal

object OptionWrapper {
  val functor: Functor[OptionWrapper] = new Functor[OptionWrapper] {
    def map[A, B](fa: OptionWrapper[A])(f: A => B) = OptionWrapper(fa.option.map(f))
  }

  implicit def optionWrapperArbitrary[A: Arbitrary]: Arbitrary[OptionWrapper[A]] =
    Arbitrary(arbitrary[Option[A]].map(OptionWrapper.apply))

  implicit def optionWrapperCogen[A: Cogen]: Cogen[OptionWrapper[A]] =
    Cogen[Option[A]].contramap(_.option)

  implicit def catsLawsExhaustiveCheckForOptionWrapper[A](
    implicit A: ExhaustiveCheck[A]
  ): ExhaustiveCheck[OptionWrapper[A]] =
    ExhaustiveCheck[Option[A]].map(OptionWrapper(_))
}
