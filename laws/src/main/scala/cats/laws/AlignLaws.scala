package cats
package laws

import cats.syntax.align._
import cats.syntax.functor._

import cats.data.Ior

/**
 * Laws that must be obeyed by any `Align`.
 */
trait AlignLaws[F[_]] extends FunctorLaws[F] {
  implicit override def F: Align[F]

  def nilLeftIdentity[A, B](fb: F[B]): IsEq[F[A Ior B]] =
    F.nil[A].align(fb) <-> fb.map(Ior.right)

  def nilRightIdentity[A, B](fa: F[A]): IsEq[F[A Ior B]] =
    fa.align(F.nil[B]) <-> fa.map(Ior.left)

  def alignSelfBoth[A](fa: F[A]): IsEq[F[A Ior A]] =
    fa.align(fa) <-> fa.map(a => Ior.both(a, a))

  def alignHomomorphism[A, B, C, D](fa: F[A], fb: F[B], f: A => C, g: B => D): IsEq[F[C Ior D]] =
    fa.map(f).align(fb.map(g)) <-> fa.align(fb).map(_.bimap(f, g))

  def alignWithConsistent[A, B, C](fa: F[A], fb: F[B], f: A Ior B => C): IsEq[F[C]] =
    fa.alignWith(fb)(f) <-> fa.align(fb).map(f)
}

object AlignLaws {
  def apply[F[_]](implicit ev: Align[F]): AlignLaws[F] =
    new AlignLaws[F] { def F: Align[F] = ev }
}
