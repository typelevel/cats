package cats.syntax

import cats.data.NonEmptyVector
import cats.{Applicative, Functor, Order, Traverse}

import scala.collection.immutable.SortedMap

trait VectorSyntax {
  implicit final def catsSyntaxVectors[A](va: Vector[A]): VectorOps[A] = new VectorOps(va)
}

final class VectorOps[A](private val va: Vector[A]) extends AnyVal {

  /**
   * Returns an Option of NonEmptyVector from a Vector
   *
   * Example:
   * {{{
   * scala> import cats.data.NonEmptyVector
   * scala> import cats.implicits._
   *
   * scala> val result1: Vector[Int] = Vector(1, 2)
   * scala> result1.toNev
   * res0: Option[NonEmptyVector[Int]] = Some(NonEmptyVector(1, 2))
   *
   * scala> val result2: Vector[Int] = Vector.empty[Int]
   * scala> result2.toNev
   * res1: Option[NonEmptyVector[Int]] = None
   * }}}
   */
  def toNev: Option[NonEmptyVector[A]] = NonEmptyVector.fromVector(va)

  /**
   * Groups elements inside this `Vector` according to the `Order` of the keys
   * produced by the given mapping function.
   *
   * {{{
   * scala> import cats.data.NonEmptyVector
   * scala> import scala.collection.immutable.SortedMap
   * scala> import cats.implicits._
   *
   * scala> val vector = Vector(12, -2, 3, -5)
   *
   * scala> val expectedResult = SortedMap(false -> NonEmptyVector.of(-2, -5), true -> NonEmptyVector.of(12, 3))
   *
   * scala> vector.groupByNev(_ >= 0) === expectedResult
   * res0: Boolean = true
   * }}}
   */
  def groupByNev[B](f: A => B)(implicit B: Order[B]): SortedMap[B, NonEmptyVector[A]] = {
    implicit val ordering: Ordering[B] = B.toOrdering
    toNev.fold(SortedMap.empty[B, NonEmptyVector[A]])(_.groupBy(f))
  }

  /**
   * Groups elements inside this `Vector` according to the `Order` of the keys
   * produced by the given mapping monadic function.
   *
   * {{{
   * scala> import cats.data.NonEmptyVector
   * scala> import scala.collection.immutable.SortedMap
   * scala> import cats.implicits._
   *
   * scala> val vector = Vector(12, -2, 3, -5)
   *
   * scala> val expectedResult = Option(SortedMap(false -> NonEmptyVector.of(-2, -5), true -> NonEmptyVector.of(12, 3)))
   *
   * scala> vector.groupByNevA(num => Option(0).map(num >= _)) === expectedResult
   * res0: Boolean = true
   * }}}
   */
  def groupByNevA[F[_], B](
    f: A => F[B]
  )(implicit F: Applicative[F], B: Order[B]): F[SortedMap[B, NonEmptyVector[A]]] = {
    implicit val ordering: Ordering[B] = B.toOrdering
    val mapFunctor = Functor[SortedMap[B, *]]
    val nevTraverse = Traverse[NonEmptyVector]

    toNev.fold(F.pure(SortedMap.empty[B, NonEmptyVector[A]])) { nev =>
      F.map(nevTraverse.traverse(nev)(a => F.tupleLeft(f(a), a))) { vector =>
        mapFunctor.map(vector.groupBy(_._2))(_.map(_._1))
      }
    }
  }
}
