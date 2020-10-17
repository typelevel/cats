package cats.syntax

import cats.data.NonEmptySeq
import scala.collection.immutable.Seq

trait SeqSyntax {
  implicit final def catsSyntaxSeqs[A](va: Seq[A]): SeqOps[A] = new SeqOps(va)
}

final class SeqOps[A](private val va: Seq[A]) extends AnyVal {
  def toNeSeq: Option[NonEmptySeq[A]] = NonEmptySeq.fromSeq(va)
}
