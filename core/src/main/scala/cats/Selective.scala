package cats

import simulacrum.{typeclass}
import scala.annotation.implicitNotFound

@implicitNotFound("Could not find an instance of Selective for ${F}")
@typeclass trait Selective[F[_]] extends Applicative[F] {
  def select[A, B](fab: F[Either[A, B]])(ff: F[A => B]): F[B]

  def branch[A, B, C](x: F[Either[A, B]])(l: F[A => C])(r: F[B => C]): F[C] = {
    val lhs = {
      val innerLhs: F[Either[A, Either[B, C]]] = map(x)(_.map(Left(_)))
      val innerRhs: F[A => Either[B, C]] = map(l)(_.andThen(Right(_)))
      select(innerLhs)(innerRhs)
    }
    select(lhs)(r)
  }

  def ifS[A](x: F[Boolean])(t: F[A])(e: F[A]): F[A] = {
    val condition: F[Either[Unit, Unit]] = map(x)(p => if (p) Left(()) else Right(()))
    val left: F[Unit => A] = map(t)(Function.const)
    val right: F[Unit => A] = map(e)(Function.const)
    branch(condition)(left)(right)
  }

  def whenS[A](fbool: F[Boolean])(fa: F[Unit]): F[Unit] =
    ifS(fbool)(fa)(unit)
}
