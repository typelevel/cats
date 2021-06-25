package cats.syntax

import cats.data.NonEmptyVector

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
}
