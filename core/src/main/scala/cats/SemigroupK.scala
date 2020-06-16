package cats

import scala.collection.immutable.{SortedMap, SortedSet}
import simulacrum.typeclass
import cats.data.Ior
import scala.annotation.implicitNotFound

/**
 * SemigroupK is a universal semigroup which operates on kinds.
 *
 * This type class is useful when its type parameter F[_] has a
 * structure that can be combined for any particular type. Thus,
 * SemigroupK is like a Semigroup for kinds (i.e. parametrized
 * types).
 *
 * A SemigroupK[F] can produce a Semigroup[F[A]] for any type A.
 *
 * Here's how to distinguish Semigroup and SemigroupK:
 *
 *  - Semigroup[A] allows two A values to be combined.
 *
 *  - SemigroupK[F] allows two F[A] values to be combined, for any A.
 *    The combination operation just depends on the structure of F,
 *    but not the structure of A.
 */
@implicitNotFound("Could not find an instance of SemigroupK for ${F}")
@typeclass trait SemigroupK[F[_]] extends Serializable { self =>

  /**
   * Combine two F[A] values.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> SemigroupK[List].combineK(List(1, 2), List(3, 4))
   * res0: List[Int] = List(1, 2, 3, 4)
   * }}}
   */
  @simulacrum.op("<+>", alias = true)
  def combineK[A](x: F[A], y: F[A]): F[A]

  /**
   * Similar to [[combineK]] but uses [[Eval]] to allow for laziness in the second
   * argument. This can allow for "short-circuiting" of computations.
   *
   * NOTE: the default implementation of `combineKEval` does not short-circuit
   * computations. For data structures that can benefit from laziness, [[SemigroupK]]
   * instances should override this method.
   *
   * In the following example, `x.combineK(bomb)` would result in an error,
   * but `combineKEval` "short-circuits" the computation. `x` is `Some` and thus the
   * result of `bomb` doesn't even need to be evaluated in order to determine
   * that the result of `combineKEval` should be `x`.
   *
   * {{{
   * scala> import cats.{Eval, Later}
   * scala> import cats.implicits._
   * scala> val bomb: Eval[Option[Int]] = Later(sys.error("boom"))
   * scala> val x: Option[Int] = Some(42)
   * scala> x.combineKEval(bomb).value
   * res0: Option[Int] = Some(42)
   * }}}
   */
  def combineKEval[A](x: F[A], y: Eval[F[A]]): Eval[F[A]] =
    y.map(yy => combineK(x, yy))

  /**
   * Given a type A, create a concrete Semigroup[F[A]].
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> val s: Semigroup[List[Int]] = SemigroupK[List].algebra[Int]
   * }}}
   */
  def algebra[A]: Semigroup[F[A]] =
    new Semigroup[F[A]] {
      def combine(x: F[A], y: F[A]): F[A] = self.combineK(x, y)
    }

  /**
   * "Compose" with a `G[_]` type to form a `SemigroupK` for `λ[α => F[G[α]]]`.
   * Note that this universally works for any `G`, because the "inner" structure
   * isn't considered when combining two instances.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> type ListOption[A] = List[Option[A]]
   * scala> val s: SemigroupK[ListOption] = SemigroupK[List].compose[Option]
   * scala> s.combineK(List(Some(1), None, Some(2)), List(Some(3), None))
   * res0: List[Option[Int]] = List(Some(1), None, Some(2), Some(3), None)
   * }}}
   */
  def compose[G[_]]: SemigroupK[λ[α => F[G[α]]]] =
    new ComposedSemigroupK[F, G] {
      val F = self
    }

  /**
   * Combines `F[A]` and `F[B]` into a `F[Either[A,B]]]`.
   *
   * Example:
   * {{{
   * scala> import cats.SemigroupK
   * scala> import cats.data.NonEmptyList
   * scala> SemigroupK[NonEmptyList].sum(NonEmptyList.one("abc"), NonEmptyList.one(2))
   * res0: NonEmptyList[Either[String,Int]] = NonEmptyList(Left(abc), Right(2))
   * }}}
   */
  def sum[A, B](fa: F[A], fb: F[B])(implicit F: Functor[F]): F[Either[A, B]] =
    combineK(F.map(fa)(Left(_)), F.map(fb)(Right(_)))
}

object SemigroupK extends ScalaVersionSpecificMonoidKInstances {
  def align[F[_]: SemigroupK: Functor]: Align[F] =
    new Align[F] {
      def align[A, B](fa: F[A], fb: F[B]): F[Ior[A, B]] =
        SemigroupK[F].combineK(Functor[F].map(fa)(Ior.left), Functor[F].map(fb)(Ior.right))
      def functor: Functor[F] = Functor[F]
    }

  implicit def catsMonoidKForOption: MonoidK[Option] = cats.instances.option.catsStdInstancesForOption
  implicit def catsMonoidKForList: MonoidK[List] = cats.instances.list.catsStdInstancesForList
  implicit def catsMonoidKForVector: MonoidK[Vector] = cats.instances.vector.catsStdInstancesForVector
  implicit def catsMonoidKForSet: MonoidK[Set] = cats.instances.set.catsStdInstancesForSet
  implicit def catsMonoidKForMap[K]: MonoidK[Map[K, *]] = cats.instances.map.catsStdMonoidKForMap[K]
  implicit def catsSemigroupKForEither[A]: SemigroupK[Either[A, *]] =
    cats.instances.either.catsStdSemigroupKForEither[A]
  implicit def catsSemigroupKForSortedSet: SemigroupK[SortedSet] = cats.instances.sortedSet.catsStdInstancesForSortedSet
  implicit def catsMonoidKForSortedMap[K: Order]: MonoidK[SortedMap[K, *]] =
    cats.instances.sortedMap.catsStdMonoidKForSortedMap[K]
  implicit def catsMonoidKForEndo: MonoidK[Endo] = cats.instances.function.catsStdMonoidKForFunction1

  /****************************************************************************/
  /* THE FOLLOWING CODE IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!!      */
  /****************************************************************************/

  /**
   * Summon an instance of [[SemigroupK]] for `F`.
   */
  @inline def apply[F[_]](implicit instance: SemigroupK[F]): SemigroupK[F] = instance

  trait Ops[F[_], A] extends Serializable {
    type TypeClassType <: SemigroupK[F]
    def self: F[A]
    val typeClassInstance: TypeClassType
    def combineK(y: F[A]): F[A] = typeClassInstance.combineK[A](self, y)
    def <+>(y: F[A]): F[A] = typeClassInstance.combineK[A](self, y)
    def combineKEval(y: Eval[F[A]]): Eval[F[A]] = typeClassInstance.combineKEval[A](self, y)
    def sum[B](fb: F[B])(implicit F: Functor[F]): F[Either[A, B]] = typeClassInstance.sum[A, B](self, fb)(F)
  }
  trait AllOps[F[_], A] extends Ops[F, A]
  trait ToSemigroupKOps extends Serializable {
    implicit def toSemigroupKOps[F[_], A](target: F[A])(implicit tc: SemigroupK[F]): Ops[F, A] {
      type TypeClassType = SemigroupK[F]
    } =
      new Ops[F, A] {
        type TypeClassType = SemigroupK[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToSemigroupKOps
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllSemigroupKOps[F[_], A](target: F[A])(implicit tc: SemigroupK[F]): AllOps[F, A] {
      type TypeClassType = SemigroupK[F]
    } =
      new AllOps[F, A] {
        type TypeClassType = SemigroupK[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }

  /****************************************************************************/
  /* END OF SIMULACRUM-MANAGED CODE                                           */
  /****************************************************************************/

}
