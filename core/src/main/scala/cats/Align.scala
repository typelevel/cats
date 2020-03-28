package cats

import simulacrum.typeclass

import cats.data.Ior

/**
 * `Align` supports zipping together structures with different shapes,
 * holding the results from either or both structures in an `Ior`.
 *
 * Must obey the laws in cats.laws.AlignLaws
 */
@typeclass trait Align[F[_]] {

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

object Align {
  def semigroup[F[_], A](implicit F: Align[F], A: Semigroup[A]): Semigroup[F[A]] = new Semigroup[F[A]] {
    def combine(x: F[A], y: F[A]): F[A] = Align[F].alignCombine(x, y)
  }

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
}
