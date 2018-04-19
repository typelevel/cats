package cats

import cats.data._

trait ErrorControl[F[_], G[_], E] extends Serializable {
  def monadErrorF: MonadError[F, E]
  def monadG: Monad[G]

  def controlError[A](fa: F[A])(f: E => G[A]): G[A]

  def accept[A](ga: G[A]): F[A]

  def control[A, B](fa: F[A])(f: Either[E, A] => G[B]): G[B] =
    monadG.flatMap(trial(fa))(f)

  def trial[A](fa: F[A]): G[Either[E, A]] =
    intercept(monadErrorF.map(fa)(Right(_): Either[E, A]))(Left(_))

  def trialT[A](fa: F[A]): EitherT[G, E, A] =
    EitherT(trial(fa))

  def intercept[A](fa: F[A])(f: E => A): G[A] =
    controlError(fa)(f andThen monadG.pure)

  def absolve[A](gea: G[Either[E, A]]): F[A] =
    monadErrorF.flatMap(accept(gea))(_.fold(monadErrorF.raiseError, monadErrorF.pure))

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
