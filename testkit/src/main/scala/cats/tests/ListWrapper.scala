package cats
package tests

import cats.instances.list._
import org.scalacheck.{Arbitrary, Cogen}
import org.scalacheck.Arbitrary.arbitrary

/** This data type exists purely for testing.
 *
 * The problem this type solves is to assist in picking up type class
 * instances that have more general constraints.
 *
 * For instance, OneAnd[*, F[_]] has a Monad instance if F[_] does too.
 * By extension, it has an Applicative instance, since Applicative is
 * a superclass of Monad.
 *
 * However, if F[_] doesn't have a Monad instance but does have an
 * Applicative instance (e.g. Validated), you can still get an
 * Applicative instance for OneAnd[*, F[_]]. These two instances
 * are different however, and it is a good idea to test to make sure
 * all "variants" of the instances are lawful.
 *
 * By providing this data type, we can have implicit search pick up
 * a specific type class instance by asking for it explicitly in a block.
 * Note that ListWrapper has no type class instances in implicit scope,
 * save for ones related to testing (e.g. Eq, Arbitrary, Cogen).
 *
 * {{{
 * {
 *   implicit val functor: Functor[ListWrapper] = ListWrapper.functor
 *   checkAll(..., ...)
 * }
 * }}}
 */
final case class ListWrapper[A](list: List[A]) extends AnyVal

object ListWrapper {
  def order[A: Order]: Order[ListWrapper[A]] = Order.by(_.list)

  def partialOrder[A: PartialOrder]: PartialOrder[ListWrapper[A]] = PartialOrder.by(_.list)

  def eqv[A: Eq]: Eq[ListWrapper[A]] = Eq.by(_.list)

  def hash[A: Hash]: Hash[ListWrapper[A]] = Hash.by(_.list)

  val traverse: Traverse[ListWrapper] = {
    val F = Traverse[List]

    new Traverse[ListWrapper] {
      def foldLeft[A, B](fa: ListWrapper[A], b: B)(f: (B, A) => B): B =
        F.foldLeft(fa.list, b)(f)
      def foldRight[A, B](fa: ListWrapper[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        F.foldRight(fa.list, lb)(f)
      def traverse[G[_], A, B](fa: ListWrapper[A])(f: A => G[B])(implicit G0: Applicative[G]): G[ListWrapper[B]] =
        G0.map(F.traverse(fa.list)(f))(ListWrapper.apply)
    }
  }

  val traverseFilter: TraverseFilter[ListWrapper] = {
    val F = TraverseFilter[List]

    new TraverseFilter[ListWrapper] {
      def traverse = ListWrapper.traverse
      def traverseFilter[G[_], A, B](
        fa: ListWrapper[A]
      )(f: A => G[Option[B]])(implicit G: Applicative[G]): G[ListWrapper[B]] =
        G.map(F.traverseFilter(fa.list)(f))(ListWrapper.apply)
    }
  }

  val functorFilter: FunctorFilter[ListWrapper] = {
    val F = FunctorFilter[List]

    new FunctorFilter[ListWrapper] {
      def functor = ListWrapper.functor
      def mapFilter[A, B](fa: ListWrapper[A])(f: A => Option[B]): ListWrapper[B] =
        ListWrapper(F.mapFilter(fa.list)(f))
    }
  }

  val foldable: Foldable[ListWrapper] = traverse

  val functor: Functor[ListWrapper] = traverse

  val invariantSemigroupal: InvariantSemigroupal[ListWrapper] = new InvariantSemigroupal[ListWrapper] {
    def product[A, B](fa: ListWrapper[A], fb: ListWrapper[B]): ListWrapper[(A, B)] =
      ListWrapper(fa.list.flatMap(a => fb.list.map(b => (a, b))))

    def imap[A, B](fa: ListWrapper[A])(f: A => B)(g: B => A) =
      ListWrapper(fa.list.map(f))
  }

  val invariant: Invariant[ListWrapper] = invariantSemigroupal

  val semigroupK: SemigroupK[ListWrapper] =
    new SemigroupK[ListWrapper] {
      def combineK[A](x: ListWrapper[A], y: ListWrapper[A]): ListWrapper[A] =
        ListWrapper(SemigroupK[List].combineK(x.list, y.list))
    }

  def semigroup[A]: Semigroup[ListWrapper[A]] = semigroupK.algebra[A]

  val alternative: Alternative[ListWrapper] = {
    val M = Alternative[List]

    new Alternative[ListWrapper] {
      def pure[A](x: A): ListWrapper[A] = ListWrapper(M.pure(x))

      def ap[A, B](f: ListWrapper[A => B])(fa: ListWrapper[A]): ListWrapper[B] =
        ListWrapper(M.ap(f.list)(fa.list))

      def empty[A]: ListWrapper[A] = ListWrapper(M.empty[A])

      def combineK[A](x: ListWrapper[A], y: ListWrapper[A]): ListWrapper[A] =
        ListWrapper(M.combineK(x.list, y.list))
    }
  }

  val monad: Monad[ListWrapper] = new Monad[ListWrapper] {
    val M = Monad[List]
    def pure[A](x: A): ListWrapper[A] = ListWrapper(x :: Nil)

    def flatMap[A, B](fa: ListWrapper[A])(f: (A) => ListWrapper[B]): ListWrapper[B] =
      ListWrapper(fa.list.flatMap(f(_).list))

    def tailRecM[A, B](a: A)(f: (A) => ListWrapper[Either[A, B]]): ListWrapper[B] =
      ListWrapper(M.tailRecM(a)(f(_).list))
  }

  val flatMap: FlatMap[ListWrapper] = monad

  val applicative: Applicative[ListWrapper] = alternative

  /** apply is taken due to ListWrapper being a case class */
  val applyInstance: Apply[ListWrapper] = alternative

  def monoidK: MonoidK[ListWrapper] = alternative

  def monoid[A]: Monoid[ListWrapper[A]] = alternative.algebra[A]

  implicit def listWrapperArbitrary[A: Arbitrary]: Arbitrary[ListWrapper[A]] =
    Arbitrary(arbitrary[List[A]].map(ListWrapper.apply))

  implicit def listWrapperCogen[A: Cogen]: Cogen[ListWrapper[A]] =
    Cogen[List[A]].contramap(_.list)

  implicit def listWrapperEq[A: Eq]: Eq[ListWrapper[A]] =
    Eq.by(_.list)
}
