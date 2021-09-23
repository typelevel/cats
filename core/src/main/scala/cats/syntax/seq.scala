package cats.syntax

import cats.data.NonEmptySeq
import scala.collection.immutable.Seq

trait SeqSyntax {
  implicit final def catsSyntaxSeqs[A](va: Seq[A]): SeqOps[A] = new SeqOps(va)
}

final class SeqOps[A](private val va: Seq[A]) extends AnyVal {

  /**
   * Returns an `Option` of `NonEmptySeq` from a `Seq`
   *
   * Example:
   * {{{
   * scala> import cats.data.NonEmptySeq
   * scala> import cats.syntax.all._
   * scala> import scala.collection.immutable.Seq
   *
   * scala> val result1: Seq[Int] = Seq(1, 2)
   * scala> result1.toNeSeq
   * res0: Option[NonEmptySeq[Int]] = Some(NonEmptySeq(1, 2))
   *
   * scala> val result2: Seq[Int] = Seq.empty[Int]
   * scala> result2.toNeSeq
   * res1: Option[NonEmptySeq[Int]] = None
   * }}}
   */
  def toNeSeq: Option[NonEmptySeq[A]] = NonEmptySeq.fromSeq(va)

  /**
   * Concatenates this `Seq` with a `NonEmptySeq` producing a new `NonEmptySeq`.
   *
   * Example:
   * {{{
   * scala> import cats.data.NonEmptySeq
   * scala> import cats.syntax.all._
   * scala> import scala.collection.immutable.Seq
   *
   * scala> Seq(1, 2, 3).concatNeSeq(NonEmptySeq.of(4, 5, 6))
   * res0: NonEmptySeq[Int] = NonEmptySeq(1, 2, 3, 4, 5, 6)
   * }}}
   */
  def concatNeSeq[AA >: A](neseq: NonEmptySeq[AA]): NonEmptySeq[AA] = neseq.prependSeq(va)
}
