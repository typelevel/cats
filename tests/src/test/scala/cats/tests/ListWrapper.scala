package cats
package tests

import cats.functor.Invariant
import cats.std.list._

import org.scalacheck.{Arbitrary, Gen}

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
  def order[A:Order]: Order[ListWrapper[A]] = Order[List[A]].on[ListWrapper[A]](_.list)

  def partialOrder[A:PartialOrder]: PartialOrder[ListWrapper[A]] = PartialOrder[List[A]].on[ListWrapper[A]](_.list)

  def eqv[A : Eq]: Eq[ListWrapper[A]] = Eq[List[A]].on[ListWrapper[A]](_.list)

  val traverse: Traverse[ListWrapper] = {
    val F = Traverse[List]

    new Traverse[ListWrapper] {
      def foldLeft[A, B](fa: ListWrapper[A], b: B)(f: (B, A) => B): B =
        F.foldLeft(fa.list, b)(f)
      def foldRight[A, B](fa: ListWrapper[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        F.foldRight(fa.list, lb)(f)
      def traverse[G[_], A, B](fa: ListWrapper[A])(f: A => G[B])(implicit G0: Applicative[G]): G[ListWrapper[B]] = {
        G0.map(F.traverse(fa.list)(f))(ListWrapper.apply)
      }
    }
  }

  val foldable: Foldable[ListWrapper] = traverse

  val functor: Functor[ListWrapper] =
    new Functor[ListWrapper] {
      def map[A, B](fa: ListWrapper[A])(f: A => B): ListWrapper[B] =
        ListWrapper(Functor[List].map(fa.list)(f))
    }

  val invariant: Invariant[ListWrapper] = functor

  val semigroupK: SemigroupK[ListWrapper] =
    new SemigroupK[ListWrapper] {
      def combineK[A](x: ListWrapper[A], y: ListWrapper[A]): ListWrapper[A] =
        ListWrapper(SemigroupK[List].combineK(x.list, y.list))
    }

  def semigroup[A]: Semigroup[ListWrapper[A]] = semigroupK.algebra[A]

  val monadCombine: MonadCombine[ListWrapper] = {
    val M = MonadCombine[List]

    new MonadCombine[ListWrapper] {
      def pure[A](x: A): ListWrapper[A] = ListWrapper(M.pure(x))

      def flatMap[A, B](fa: ListWrapper[A])(f: A => ListWrapper[B]): ListWrapper[B] =
        ListWrapper(M.flatMap(fa.list)(a => f(a).list))

      def empty[A]: ListWrapper[A] = ListWrapper(M.empty[A])

      def combineK[A](x: ListWrapper[A], y: ListWrapper[A]): ListWrapper[A] =
        ListWrapper(M.combineK(x.list, y.list))
    }
  }

  val monadRec: MonadRec[ListWrapper] = {
    val M = MonadRec[List]

    new MonadRec[ListWrapper] {
      def pure[A](x: A): ListWrapper[A] = ListWrapper(M.pure(x))
      def flatMap[A, B](fa: ListWrapper[A])(f: A => ListWrapper[B]): ListWrapper[B] = ListWrapper(M.flatMap(fa.list)(a => f(a).list))
      def tailRecM[A, B](a: A)(f: A => ListWrapper[cats.data.Xor[A,B]]): ListWrapper[B] =
        ListWrapper(M.tailRecM(a)(a => f(a).list))
    }
  }

  val flatMapRec: FlatMapRec[ListWrapper] = monadRec

  val monad: Monad[ListWrapper] = monadCombine

  val applicative: Applicative[ListWrapper] = monadCombine

  /** apply is taken due to ListWrapper being a case class */
  val applyInstance: Apply[ListWrapper] = monadCombine

  def monoidK: MonoidK[ListWrapper] = monadCombine

  def monadFilter: MonadFilter[ListWrapper] = monadCombine

  def alternative: Alternative[ListWrapper] = monadCombine

  def monoid[A]: Monoid[ListWrapper[A]] = monadCombine.algebra[A]

  implicit def listWrapperArbitrary[A](implicit A: Arbitrary[A]): Arbitrary[ListWrapper[A]] =
    Arbitrary(
      for {
        size <- Gen.chooseNum(0, 3)
        list <- Gen.listOfN(size, A.arbitrary)
      } yield ListWrapper(list))

  implicit def listWrapperEq[A: Eq]: Eq[ListWrapper[A]] = Eq.by(_.list)
}
