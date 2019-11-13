package cats
package syntax

trait TraverseFilterSyntax extends TraverseFilter.ToTraverseFilterOps

private[syntax] trait TraverseFilterSyntaxBinCompat0 {
  implicit def toSequenceFilterOps[F[_], G[_], A](fgoa: F[G[Option[A]]]): SequenceFilterOps[F, G, A] =
    new SequenceFilterOps(fgoa)
}

final class SequenceFilterOps[F[_], G[_], A](private val fgoa: F[G[Option[A]]]) extends AnyVal {

  /**
   * {{{
   * scala> import cats.implicits._
   * scala> val a: List[Either[String, Option[Int]]] = List(Right(Some(1)), Right(Some(5)), Right(Some(3)))
   * scala> val b: Either[String, List[Int]] = a.sequenceFilter
   * b: Either[String, List[Int]] = Right(List(1, 5, 3))
   * }}}
   * */
  def sequenceFilter(implicit F: TraverseFilter[F], G: Applicative[G]): G[F[A]] = F.traverseFilter(fgoa)(identity)
}
