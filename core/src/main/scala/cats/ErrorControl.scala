package cats

import cats.data._

/**
 * A type class for principled error handling.
 * `ErrorControl` is designed to be a supplement to `MonadError` with more precise typing.
 * It is defined as a relationship between an error-handling type `F[A]` and a non-error-handling type `G[A]`.
 * This means a value of `F[A]` is able to produce either a value of `A` or an error of type `E`.
 * Unlike `MonadError`'s `handleError` method, the `controlError` function defined in this type class
 * will yield a value that free of any errors, since they've all been handled.
 *
 * Must adhere to the laws defined in cats.laws.ErrorControlLaws.
 */
trait ErrorControl[F[_], G[_], E] extends Serializable {
  /**
   * The MonadError instance for F[_]
   */
  def monadErrorF: MonadError[F, E]

  /**
   * The Monad instance for G[_]
   */
  def monadG: Monad[G]

  /**
   * Handle any error and recover from it, by mapping it to an
   * error-free `G[A]` value.
   *
   * Similar to `handleErrorWith` on `ApplicativeError`
   */
  def controlError[A](fa: F[A])(f: E => G[A]): G[A]

  /**
   * Injects this error-free `G[A]` value into an `F[A]`.
   */
  def accept[A](ga: G[A]): F[A]

  /**
   * Transform this `F[A]` value, by mapping either the error or the valid value to a new error-free `G[B]`.
   */
  def control[A, B](fa: F[A])(f: Either[E, A] => G[B]): G[B] =
    monadG.flatMap(trial(fa))(f)

  /**
   * Handle errors by turning them into [[scala.util.Either]] values inside `G`.
   *
   * If there is no error, then an `scala.util.Right` value will be returned.
   *
   * All non-fatal errors should be handled by this method.
   *
   * Similar to `attempt` on `ApplicativeError`.
   */
  def trial[A](fa: F[A]): G[Either[E, A]] =
    intercept(monadErrorF.map(fa)(Right(_): Either[E, A]))(Left(_))

  /**
   * Like [[trial]], but returns inside the [[cats.data.EitherT]] monad transformer instead.
   */
  def trialT[A](fa: F[A]): EitherT[G, E, A] =
    EitherT(trial(fa))

  /**
   * Handle any error and recover from it, by mapping it to `A`.
   *
   * Similar to `handleError` on `ApplicativeError`
   */
  def intercept[A](fa: F[A])(f: E => A): G[A] =
    controlError(fa)(f andThen monadG.pure)

  /**
   * The inverse of [[trial]].
   */
  def absolve[A](gea: G[Either[E, A]]): F[A] =
    monadErrorF.flatMap(accept(gea))(_.fold(monadErrorF.raiseError, monadErrorF.pure))

  /**
   * Turns a successful value into an error specified by the `error` function if it does not satisfy a given predicate.
   */
  def assure[A](ga: G[A])(error: A => Option[E])(predicate: A => Boolean): F[A] =
    monadErrorF.flatMap(accept(ga))(a =>
      if (predicate(a)) monadErrorF.pure(a) else error(a) match {
        case Some(e) => monadErrorF.raiseError(e)
        case None => monadErrorF.pure(a)
      })

}

object ErrorControl {

  def apply[F[_], G[_], E](implicit ev: ErrorControl[F, G, E]): ErrorControl[F, G, E] = ev


  implicit def catsErrorControlForStateT[F[_], G[_], S, E]
  (implicit E: ErrorControl[F, G, E]): ErrorControl[StateT[F, S, ?], StateT[G, S, ?], E] =
    new ErrorControl[StateT[F, S, ?], StateT[G, S, ?], E] {
      implicit val F: MonadError[F, E] = E.monadErrorF
      implicit val G: Monad[G] = E.monadG

      val monadErrorF: MonadError[StateT[F, S, ?], E] = IndexedStateT.catsDataMonadErrorForIndexedStateT
      val monadG: Monad[StateT[G, S, ?]] = IndexedStateT.catsDataMonadForIndexedStateT

      def accept[A](ga: StateT[G, S, A]): StateT[F, S, A] = ga.mapK(new (G ~> F) {
        def apply[T](ga: G[T]): F[T] = E.accept(ga)
      })

      def controlError[A](fa: StateT[F, S, A])(f: E => StateT[G, S, A]): StateT[G, S, A] =
        IndexedStateT(s => E.controlError(fa.run(s))(e => f(e).run(s)))

    }


  implicit def catsErrorControlForKleisli[F[_], G[_], R, E]
  (implicit E: ErrorControl[F, G, E]): ErrorControl[Kleisli[F, R, ?], Kleisli[G, R, ?], E] =
    new ErrorControl[Kleisli[F, R, ?], Kleisli[G, R, ?], E] {
      implicit val F: MonadError[F, E] = E.monadErrorF
      implicit val G: Monad[G] = E.monadG

      val monadErrorF: MonadError[Kleisli[F, R, ?], E] = Kleisli.catsDataMonadErrorForKleisli
      val monadG: Monad[Kleisli[G, R, ?]] = Kleisli.catsDataMonadForKleisli

      def accept[A](ga: Kleisli[G, R, A]): Kleisli[F, R, A] = ga.mapK(new (G ~> F) {
        def apply[T](ga: G[T]): F[T] = E.accept(ga)
      })

      def controlError[A](fa: Kleisli[F, R, A])(f: E => Kleisli[G, R, A]): Kleisli[G, R, A] =
        Kleisli(r => E.controlError(fa.run(r))(e => f(e).run(r)))

    }


  implicit def catsErrorControlForWriterT[F[_], G[_], L: Monoid, E]
  (implicit M: ErrorControl[F, G, E]): ErrorControl[WriterT[F, L, ?], WriterT[G, L, ?], E] =
    new ErrorControl[WriterT[F, L, ?], WriterT[G, L, ?], E] {
      implicit val F: MonadError[F, E] = M.monadErrorF
      implicit val G: Monad[G] = M.monadG

      val monadErrorF: MonadError[WriterT[F, L, ?], E] = WriterT.catsDataMonadErrorForWriterT
      val monadG: Monad[WriterT[G, L, ?]] = WriterT.catsDataMonadForWriterT

      def accept[A](ga: WriterT[G, L, A]): WriterT[F, L, A] = ga.mapK(new (G ~> F) {
        def apply[T](ga: G[T]): F[T] = M.accept(ga)
      })

      def controlError[A](fa: WriterT[F, L, A])(f: E => WriterT[G, L, A]): WriterT[G, L, A] =
        WriterT(M.controlError(fa.run)(e => f(e).run))

    }

}
