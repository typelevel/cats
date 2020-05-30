package cats
package laws

import cats.syntax.align._
import cats.syntax.functor._

import cats.data.Ior
import cats.data.Ior.{Both, Left, Right}

/**
 * Laws that must be obeyed by any `Align`.
 */
trait AlignLaws[F[_]] {
  implicit def F: Align[F]

  implicit val functor: Functor[F] = F.functor

  def alignAssociativity[A, B, C](fa: F[A], fb: F[B], fc: F[C]): IsEq[F[Ior[Ior[A, B], C]]] =
    fa.align(fb).align(fc) <-> fa.align(fb.align(fc)).map(assoc)

  def alignHomomorphism[A, B, C, D](fa: F[A], fb: F[B], f: A => C, g: B => D): IsEq[F[Ior[C, D]]] =
    fa.map(f).align(fb.map(g)) <-> fa.align(fb).map(_.bimap(f, g))

  def alignWithConsistent[A, B, C](fa: F[A], fb: F[B], f: A Ior B => C): IsEq[F[C]] =
    fa.alignWith(fb)(f) <-> fa.align(fb).map(f)

  private def assoc[A, B, C](x: Ior[A, Ior[B, C]]): Ior[Ior[A, B], C] =
    x match {
      case Left(a) => Left(Left(a))
      case Right(bc) =>
        bc match {
          case Left(b)    => Left(Right(b))
          case Right(c)   => Right(c)
          case Both(b, c) => Both(Right(b), c)
        }
      case Both(a, bc) =>
        bc match {
          case Left(b)    => Left(Both(a, b))
          case Right(c)   => Both(Left(a), c)
          case Both(b, c) => Both(Both(a, b), c)
        }
    }
}

object AlignLaws {
  def apply[F[_]](implicit ev: Align[F]): AlignLaws[F] =
    new AlignLaws[F] { def F: Align[F] = ev }
}
