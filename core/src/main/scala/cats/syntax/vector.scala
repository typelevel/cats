package cats.syntax

import cats.data.NonEmptyVector

trait VectorSyntax {
  implicit final def catsSyntaxVectors[A](va: Vector[A]): VectorOps[A] = new VectorOps(va)
}

final class VectorOps[A](val va: Vector[A]) extends AnyVal {
  def toNev: Option[NonEmptyVector[A]] = NonEmptyVector.fromVector(va)
}
