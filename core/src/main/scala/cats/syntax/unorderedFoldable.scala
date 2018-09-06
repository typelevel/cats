package cats
package syntax

import cats.kernel.CommutativeMonoid

trait UnorderedFoldableSyntax extends UnorderedFoldable.ToUnorderedFoldableOps {
  implicit final def catsSyntaxUnorderedFoldableOps[F[_]: UnorderedFoldable, A](fa: F[A]): UnorderedFoldableOps[F, A] =
    new UnorderedFoldableOps[F, A](fa)
}

final class UnorderedFoldableOps[F[_], A](val fa: F[A]) extends AnyVal {
  private def orderMaxCommutativeMonoidBy[B](f: A => B)(implicit ev: Order[B]): CommutativeMonoid[Option[A]] =
    new CommutativeMonoid[Option[A]] {
      override def empty = None
      override def combine(optX: Option[A], optY: Option[A]): Option[A] = (optX, optY) match {
        case (None, y) => y
        case (x, None) => x
        case (Some(x), Some(y)) => if (ev.gteqv(f(x), f(y))) Some(x) else Some(y)
      }
    }

  private def orderMinCommutativeMonoidBy[B](f: A => B)(implicit ev: Order[B]): CommutativeMonoid[Option[A]] =
    new CommutativeMonoid[Option[A]] {
      override def empty = None
      override def combine(optX: Option[A], optY: Option[A]): Option[A] = (optX, optY) match {
        case (None, y) => y
        case (x, None) => x
        case (Some(x), Some(y)) => if (ev.lteqv(f(x), f(y))) Some(x) else Some(y)
      }
    }

  def maxByOption[B](f: A => B)(implicit F: UnorderedFoldable[F], B: Order[B]): Option[A] =
    F.unorderedFoldMap(fa)(Option.apply)(orderMaxCommutativeMonoidBy(f))

  def minByOption[B](f: A => B)(implicit F: UnorderedFoldable[F], B: Order[B]): Option[A] =
    F.unorderedFoldMap(fa)(Option.apply)(orderMinCommutativeMonoidBy(f))
}
