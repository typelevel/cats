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
   * Align[List].align(List(1, 2), List(10, 11, 12)) = List(Ior.Both(1, 10), Ior.Both(2, 11), Ior.Right(12))
   */
  def align[A, B](fa: F[A], fb: F[B]): F[Ior[A, B]]

  /**
   * Combines elements similarly to `align`, using the provided function to compute the results.
   */
  def alignWith[A, B, C](fa: F[A], fb: F[B])(f: Ior[A, B] => C): F[C] =
    functor.map(align(fa, fb))(f)

  /**
   * Align two structures with the same element, combining results according to their semigroup instances.
   */
  def alignCombine[A: Semigroup](fa1: F[A], fa2: F[A]): F[A] =
    alignWith(fa1, fa2)(_.merge)

  /**
   * Same as `align`, but forgets from the type that one of the two elements must be present.
   */
  def padZip[A, B](fa: F[A], fb: F[B]): F[(Option[A], Option[B])] =
    alignWith(fa, fb)(_.pad)

  /**
   * Same as `alignWith`, but forgets from the type that one of the two elements must be present.
   */
  def padZipWith[A, B, C](fa: F[A], fb: F[B])(f: (Option[A], Option[B]) => C): F[C] =
    alignWith(fa, fb)(ior => Function.tupled(f)(ior.pad))
}
