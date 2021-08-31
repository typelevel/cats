package cats
package laws

import cats.platform.Platform

/**
 * Laws that must be obeyed by any `Defer`.
 */
trait DeferLaws[F[_]] {
  implicit def F: Defer[F]

  def deferIdentity[A](fa: Unit => F[A]): IsEq[F[A]] =
    F.defer(fa(())) <-> fa(())

  def deferDoesNotEvaluate[A](fa: Unit => F[A]): IsEq[Boolean] = {
    var evaluated = false
    val deferUnit = F.defer {
      evaluated = true;
      fa(())
    }
    evaluated <-> false
  }

  def deferIsStackSafe[A](fa: Unit => F[A]): IsEq[F[A]] = {
    def loop(c: Int): F[A] =
      if (c <= 0) F.defer(fa(()))
      else F.defer(loop(c - 1))

    val cnt = if (Platform.isJvm) 20000 else 2000
    loop(cnt) <-> (fa(()))
  }

  def deferMatchesFix[A](fa: Unit => F[A]): IsEq[F[A]] = {
    val defered = F.defer(fa(()))
    val viaFix = F.fix[A](_ => fa(()))
    defered <-> viaFix
  }
}

object DeferLaws {
  def apply[F[_]](implicit ev: Defer[F]): DeferLaws[F] =
    new DeferLaws[F] { def F: Defer[F] = ev }
}
