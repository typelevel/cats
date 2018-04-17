package cats

import cats.data.EitherT

trait ErrorControl[F[_], G[_], E] extends Serializable {
  val monadErrorF: MonadError[F, E]
  val monadG: Monad[G]

  def controlError[A](fa: F[A])(f: E => G[A]): G[A]

  def accept[A](ga: G[A]): F[A]

  def trial[A](fa: F[A]): G[Either[E, A]] =
    intercept(monadErrorF.map(fa)(Right(_): Either[E, A]))(Left(_))

  def trialT[A](fa: F[A]): EitherT[G, E, A] =
    EitherT(trial(fa))

  def intercept[A](fa: F[A])(f: E => A): G[A] =
    controlError(fa)(f andThen monadG.pure)

  def absolve[A](gea: G[Either[E, A]]): F[A] =
    monadErrorF.flatMap(accept(gea))(_.fold(monadErrorF.raiseError, monadErrorF.pure))

  def assure[A](ga: G[A])(error: => E)(predicate: A => Boolean): F[A] =
    assureOr(ga)(_ => error)(predicate)

  def assureOr[A](ga: G[A])(error: A => E)(predicate: A => Boolean): F[A] =
    monadErrorF.flatMap(accept(ga))(a =>
      if (predicate(a)) monadErrorF.pure(a) else monadErrorF.raiseError(error(a)))

}

object ErrorControl {

  def apply[F[_], G[_], E](implicit ev: ErrorControl[F, G, E]): ErrorControl[F, G, E] = ev

}
