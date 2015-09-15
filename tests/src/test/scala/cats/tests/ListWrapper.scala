package cats
package tests

import cats.data.OneAnd
import cats.std.list._
import cats.laws.discipline.ArbitraryK
import cats.laws.discipline.arbitrary.oneAndArbitrary

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

/** This data type exists purely for testing.
  *
  * The problem this type solves is to assist in picking up type class
  * instances that have more general constraints.
  *
  * For instance, OneAnd[?, F[_]] has a Monad instance if F[_] does too.
  * By extension, it has an Applicative instance, since Applicative is
  * a superclass of Monad.
  *
  * However, if F[_] doesn't have a Monad instance but does have an
  * Applicative instance (e.g. Validated), you can still get an
  * Applicative instance for OneAnd[?, F[_]]. These two instances
  * are different however, and it is a good idea to test to make sure
  * all "variants" of the instances are lawful.
  *
  * By providing this data type, we can have implicit search pick up
  * a specific type class instance by asking for it explicitly in a block.
  * Note that ListWrapper has no type class instances in implicit scope,
  * save for ones related to testing (e.g. Eq and Arbitrary).
  *
  * {{{
  * {
  *   implicit val functor = ListWrapper.functor
  *   checkAll(..., ...)
  * }
  * }}}
  */

final case class ListWrapper[A](list: List[A]) extends AnyVal

object ListWrapper {
  def eqv[A : Eq]: Eq[ListWrapper[A]] =
    new Eq[ListWrapper[A]] {
      def eqv(x: ListWrapper[A], y: ListWrapper[A]): Boolean =
        Eq[List[A]].eqv(x.list, y.list)
    }

  def foldable: Foldable[ListWrapper] =
    new Foldable[ListWrapper] {
      def foldLeft[A, B](fa: ListWrapper[A], b: B)(f: (B, A) => B): B =
        Foldable[List].foldLeft(fa.list, b)(f)

      def foldRight[A, B](fa: ListWrapper[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        Foldable[List].foldRight(fa.list, lb)(f)
    }

  def functor: Functor[ListWrapper] =
    new Functor[ListWrapper] {
      def map[A, B](fa: ListWrapper[A])(f: A => B): ListWrapper[B] =
        ListWrapper(Functor[List].map(fa.list)(f))
    }

  def semigroupK: SemigroupK[ListWrapper] =
    new SemigroupK[ListWrapper] {
      def combine[A](x: ListWrapper[A], y: ListWrapper[A]): ListWrapper[A] =
        ListWrapper(SemigroupK[List].combine(x.list, y.list))
    }

  def monadCombine: MonadCombine[ListWrapper] = {
    val M = MonadCombine[List]

    new MonadCombine[ListWrapper] {
      def pure[A](x: A): ListWrapper[A] = ListWrapper(M.pure(x))

      def flatMap[A, B](fa: ListWrapper[A])(f: A => ListWrapper[B]): ListWrapper[B] =
        ListWrapper(M.flatMap(fa.list)(a => f(a).list))

      def empty[A]: ListWrapper[A] = ListWrapper(M.empty[A])

      def combine[A](x: ListWrapper[A], y: ListWrapper[A]): ListWrapper[A] =
        ListWrapper(M.combine(x.list, y.list))
    }
  }

  implicit def listWrapperArbitrary[A: Arbitrary]: Arbitrary[ListWrapper[A]] =
    Arbitrary(arbitrary[List[A]].map(ListWrapper.apply))

  implicit val listWrapperArbitraryK: ArbitraryK[ListWrapper] =
    new ArbitraryK[ListWrapper] {
      def synthesize[A: Arbitrary]: Arbitrary[ListWrapper[A]] = implicitly
    }

  implicit val listWrapperOneAndArbitraryK: ArbitraryK[OneAnd[?, ListWrapper]] =
    new ArbitraryK[OneAnd[?, ListWrapper]] {
      def synthesize[A: Arbitrary]: Arbitrary[OneAnd[A, ListWrapper]] = implicitly
    }

  implicit def listWrapperEq[A: Eq]: Eq[ListWrapper[A]] = Eq.by(_.list)
}
