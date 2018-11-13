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
}

final class IfAOps[F[_]](val fa: F[Boolean]) extends AnyVal {

  /**
   * A conditional lifted into the `F` context.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   *
   * scala> val b1: Option[Boolean] = Some(true)
   * scala> val asInt1: Option[Int] = b1.ifA(Some(1), Some(0))
   * scala> asInt1.get
   * res0: Int = 1
   *
   * scala> val b2: Option[Boolean] = Some(false)
   * scala> val asInt2: Option[Int] = b2.ifA(Some(1), Some(0))
   * scala> asInt2.get
   * res1: Int = 0
   *
   * scala> val b3: Option[Boolean] = Some(true)
   * scala> val asInt3: Option[Int] = b3.ifA(Some(1), None)
   * asInt2: Option[Int] = None
   *
   * }}}
   */
  def ifA[A](ifTrue: F[A], ifFalse: => F[A])(implicit F: Apply[F]): F[A] = F.ifA(fa)(ifTrue, ifFalse)
}

final class ApplyOps[F[_], A](val fa: F[A]) extends AnyVal {

  /** Alias for [[Apply.productR]]. */
  @deprecated("Use *> or productR instead.", "1.0.0-RC2")
  @inline def followedBy[B](fb: F[B])(implicit F: Apply[F]): F[B] =
    F.productR(fa)(fb)

  /** Alias for [[Apply.productL]]. */
  @deprecated("Use <* or productL instead.", "1.0.0-RC2")
  @inline def forEffect[B](fb: F[B])(implicit F: Apply[F]): F[A] =
    F.productL(fa)(fb)
}
