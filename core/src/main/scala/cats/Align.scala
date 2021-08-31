package cats

import simulacrum.typeclass

import cats.data.Ior
import scala.collection.immutable.{Seq, SortedMap}
import scala.annotation.implicitNotFound

/**
 * `Align` supports zipping together structures with different shapes,
 * holding the results from either or both structures in an `Ior`.
 *
 * Must obey the laws in cats.laws.AlignLaws
 */
@implicitNotFound("Could not find an instance of Align for ${F}")
@typeclass trait Align[F[_]] extends Serializable {

  def functor: Functor[F]

  /**
   * Pairs elements of two structures along the union of their shapes, using `Ior` to hold the results.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> import cats.data.Ior
   * scala> Align[List].align(List(1, 2), List(10, 11, 12))
   * res0: List[Ior[Int, Int]] = List(Both(1,10), Both(2,11), Right(12))
   * }}}
   */
  def align[A, B](fa: F[A], fb: F[B]): F[Ior[A, B]]

  /**
   * Combines elements similarly to `align`, using the provided function to compute the results.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> Align[List].alignWith(List(1, 2), List(10, 11, 12))(_.mergeLeft)
   * res0: List[Int] = List(1, 2, 12)
   * }}}
   */
  def alignWith[A, B, C](fa: F[A], fb: F[B])(f: Ior[A, B] => C): F[C] =
    functor.map(align(fa, fb))(f)

  /**
   * Align two structures with the same element, combining results according to their semigroup instances.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> Align[List].alignCombine(List(1, 2), List(10, 11, 12))
   * res0: List[Int] = List(11, 13, 12)
   * }}}
   */
  def alignCombine[A: Semigroup](fa1: F[A], fa2: F[A]): F[A] =
    alignWith(fa1, fa2)(_.merge)

  /**
   * Align two structures with the same element, combining results according to the given function.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> Align[List].alignMergeWith(List(1, 2), List(10, 11, 12))(_ + _)
   * res0: List[Int] = List(11, 13, 12)
   * }}}
   */
  def alignMergeWith[A](fa1: F[A], fa2: F[A])(f: (A, A) => A): F[A] =
    functor.map(align(fa1, fa2))(_.mergeWith(f))

  /**
   * Same as `align`, but forgets from the type that one of the two elements must be present.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> Align[List].padZip(List(1, 2), List(10))
   * res0: List[(Option[Int], Option[Int])] = List((Some(1),Some(10)), (Some(2),None))
   * }}}
   */
  def padZip[A, B](fa: F[A], fb: F[B]): F[(Option[A], Option[B])] =
    alignWith(fa, fb)(_.pad)

  /**
   * Same as `alignWith`, but forgets from the type that one of the two elements must be present.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> Align[List].padZipWith(List(1, 2), List(10, 11, 12))(_ |+| _)
   * res0: List[Option[Int]] = List(Some(11), Some(13), Some(12))
   * }}}
   */
  def padZipWith[A, B, C](fa: F[A], fb: F[B])(f: (Option[A], Option[B]) => C): F[C] =
    alignWith(fa, fb) { ior =>
      val (oa, ob) = ior.pad
      f(oa, ob)
    }

  /**
   * Pairs elements of two structures along the union of their shapes, using placeholders for missing values.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> Align[List].zipAll(List(1, 2), List(10, 11, 12), 20, 21)
   * res0: List[(Int, Int)] = List((1,10), (2,11), (20,12))
   * }}}
   */
  def zipAll[A, B](fa: F[A], fb: F[B], a: A, b: B): F[(A, B)] =
    alignWith(fa, fb) {
      case Ior.Left(x)    => (x, b)
      case Ior.Right(y)   => (a, y)
      case Ior.Both(x, y) => (x, y)
    }
}

object Align extends ScalaVersionSpecificAlignInstances {
  def semigroup[F[_], A](implicit F: Align[F], A: Semigroup[A]): Semigroup[F[A]] =
    new Semigroup[F[A]] {
      def combine(x: F[A], y: F[A]): F[A] = Align[F].alignCombine(x, y)
    }

  implicit def catsAlignForList: Align[List] = cats.instances.list.catsStdInstancesForList
  implicit def catsAlignForOption: Align[Option] = cats.instances.option.catsStdInstancesForOption
  implicit def catsAlignForSeq: Align[Seq] = cats.instances.seq.catsStdInstancesForSeq
  implicit def catsAlignForVector: Align[Vector] = cats.instances.vector.catsStdInstancesForVector
  implicit def catsAlignForMap[K]: Align[Map[K, *]] = cats.instances.map.catsStdInstancesForMap[K]
  implicit def catsAlignForSortedMap[K]: Align[SortedMap[K, *]] =
    cats.instances.sortedMap.catsStdInstancesForSortedMap[K]
  implicit def catsAlignForEither[A]: Align[Either[A, *]] = cats.instances.either.catsStdInstancesForEither[A]

  private[cats] def alignWithIterator[A, B, C](fa: Iterable[A], fb: Iterable[B])(f: Ior[A, B] => C): Iterator[C] =
    new Iterator[C] {
      private[this] val iterA = fa.iterator
      private[this] val iterB = fb.iterator
      def hasNext: Boolean = iterA.hasNext || iterB.hasNext
      def next(): C =
        f(
          if (iterA.hasNext && iterB.hasNext) Ior.both(iterA.next(), iterB.next())
          else if (iterA.hasNext) Ior.left(iterA.next())
          else Ior.right(iterB.next())
        )
    }

  /* ======================================================================== */
  /* THE FOLLOWING CODE IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!!      */
  /* ======================================================================== */

  /**
   * Summon an instance of [[Align]] for `F`.
   */
  @inline def apply[F[_]](implicit instance: Align[F]): Align[F] = instance

  @deprecated("Use cats.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllAlignOps[F[_], A](target: F[A])(implicit tc: Align[F]): AllOps[F, A] {
      type TypeClassType = Align[F]
    } =
      new AllOps[F, A] {
        type TypeClassType = Align[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  trait Ops[F[_], A] extends Serializable {
    type TypeClassType <: Align[F]
    def self: F[A]
    val typeClassInstance: TypeClassType
    def align[B](fb: F[B]): F[Ior[A, B]] = typeClassInstance.align[A, B](self, fb)
    def alignWith[B, C](fb: F[B])(f: Ior[A, B] => C): F[C] = typeClassInstance.alignWith[A, B, C](self, fb)(f)
    def alignCombine(fa2: F[A])(implicit ev$1: Semigroup[A]): F[A] = typeClassInstance.alignCombine[A](self, fa2)
    def alignMergeWith(fa2: F[A])(f: (A, A) => A): F[A] = typeClassInstance.alignMergeWith[A](self, fa2)(f)
    def padZip[B](fb: F[B]): F[(Option[A], Option[B])] = typeClassInstance.padZip[A, B](self, fb)
    def padZipWith[B, C](fb: F[B])(f: (Option[A], Option[B]) => C): F[C] =
      typeClassInstance.padZipWith[A, B, C](self, fb)(f)
    def zipAll[B](fb: F[B], a: A, b: B): F[(A, B)] = typeClassInstance.zipAll[A, B](self, fb, a, b)
  }
  trait AllOps[F[_], A] extends Ops[F, A]
  trait ToAlignOps extends Serializable {
    implicit def toAlignOps[F[_], A](target: F[A])(implicit tc: Align[F]): Ops[F, A] {
      type TypeClassType = Align[F]
    } =
      new Ops[F, A] {
        type TypeClassType = Align[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToAlignOps

  /* ======================================================================== */
  /* END OF SIMULACRUM-MANAGED CODE                                           */
  /* ======================================================================== */

}
