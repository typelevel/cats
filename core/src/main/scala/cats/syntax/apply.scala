package cats
package syntax

trait ApplySyntax extends TupleSemigroupalSyntax {
  implicit final def catsSyntaxApply[F[_], A](fa: F[A])(implicit F: Apply[F]): Apply.Ops[F, A] =
    new Apply.Ops[F, A] {
      type TypeClassType = Apply[F]

      val self = fa
      val typeClassInstance = F
    }

  implicit final def catsSyntaxApplyOps[F[_], A](fa: F[A]): ApplyOps[F, A] =
    new ApplyOps(fa)

  implicit final def catsSyntaxApplyApOps[F[_], A, B](ff: F[A => B]): ApplyApOps[F, A, B] =
    new ApplyApOps(ff)
}

final class ApplyOps[F[_], A](val fa: F[A]) extends AnyVal {
  /** Alias for [[*>]]. */
  @deprecated("Use *> or apR instead.", "1.0.0-RC2")
  @inline def followedBy[B](fb: F[B])(implicit F: Apply[F]): F[B] =
    F.apR(fa)(fb)

  /** Alias for [[<*]]. */
  @deprecated("Use <* or apL instead.", "1.0.0-RC2")
  @inline def forEffect[B](fb: F[B])(implicit F: Apply[F]): F[A] =
    F.apL(fa)(fb)

  /** Alias for [[Applicative.apR]]. */
  @inline def *>[B](fb: F[B])(implicit F: Apply[F]): F[B] =
    F.apR(fa)(fb)

  /** Alias for [[Applicative.apL]]. */
  @inline def <*[B](fb: F[B])(implicit F: Apply[F]): F[A] =
    F.apL(fa)(fb)
}

final class ApplyApOps[F[_], A, B](val ff: F[A => B]) extends AnyVal {
  /** Alias for [[Applicative.ap]]. */
  @inline def <*>(fa: F[A])(implicit F: Apply[F]): F[B] =
    F.ap(ff)(fa)
}
