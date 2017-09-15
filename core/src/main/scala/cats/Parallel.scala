package cats

import cats.arrow.FunctionK

/**
  * Some types that form a Monad, are also capable of forming an Applicative that supports parallel composition.
  * The Parallel type class allows us to represent this relationship.
  */
trait Parallel[M[_], F[_]] extends Serializable {
  /**
    * The applicative instance for F[_]
    */
  def applicative: Applicative[F]

  /**
    * The monad instance for M[_]
    */
  def monad: Monad[M]

  /**
    * Natural Transformation from the parallel Applicative F[_] to the sequential Monad M[_].
    */
  def sequential: F ~> M

  /**
    * Natural Transformation from the sequential Monad M[_] to the parallel Applicative F[_].
    */
  def parallel: M ~> F

  /**
    * Provides an `ApplicativeError[F, E]` instance for any F, that has a `Parallel[M, F]`
    * and a `MonadError[M, E]` instance.
    * I.e. if you have a type M[_], that supports parallel composition through type F[_],
    * then you can get `ApplicativeError[F, E]` from `MonadError[M, E]`.
    */
  def applicativeError[E](implicit E: MonadError[M, E]): ApplicativeError[F, E] = new ApplicativeError[F, E] {

    def raiseError[A](e: E): F[A] =
      parallel(MonadError[M, E].raiseError(e))

    def handleErrorWith[A](fa: F[A])(f: (E) => F[A]): F[A] = {
      val ma = MonadError[M, E].handleErrorWith(sequential(fa))(f andThen sequential.apply)
      parallel(ma)
    }

    def pure[A](x: A): F[A] = applicative.pure(x)

    def ap[A, B](ff: F[(A) => B])(fa: F[A]): F[B] = applicative.ap(ff)(fa)
  }
}

object Parallel extends ParallelArityFunctions {

  def apply[M[_], F[_]](implicit P: Parallel[M, F]): Parallel[M, F] = P

  /**
    * Like `Traverse[A].sequence`, but uses the applicative instance
    * corresponding to the Parallel instance instead.
    */
  def parSequence[T[_]: Traverse, M[_], F[_], A]
  (tma: T[M[A]])(implicit P: Parallel[M, F]): M[T[A]] = {
    implicit val F = P.applicative
    implicit val M = P.monad
    val fta: F[T[A]] = Traverse[T].traverse(tma)(P.parallel.apply)
    P.sequential(fta)
  }

  /**
    * Like `Traverse[A].traverse`, but uses the applicative instance
    * corresponding to the Parallel instance instead.
    */
  def parTraverse[T[_]: Traverse, M[_], F[_], A, B]
  (ta: T[A])(f: A => M[B])(implicit P: Parallel[M, F]): M[T[B]] = {
    implicit val F = P.applicative
    implicit val M = P.monad
    val gtb: F[T[B]] = Traverse[T].traverse(ta)(f andThen P.parallel.apply)
    P.sequential(gtb)
  }

  /**
    * Like `Foldable[A].sequence_`, but uses the applicative instance
    * corresponding to the Parallel instance instead.
    */
  def parSequence_[T[_]: Foldable, M[_], F[_], A]
  (tma: T[M[A]])(implicit P: Parallel[M, F]): M[Unit] = {
    implicit val F = P.applicative
    implicit val M = P.monad
    val fu: F[Unit] = Foldable[T].traverse_(tma)(P.parallel.apply)
    P.sequential(fu)
  }

  /**
    * Like `Foldable[A].traverse_`, but uses the applicative instance
    * corresponding to the Parallel instance instead.
    */
  def parTraverse_[T[_]: Foldable, M[_], F[_], A, B]
  (ta: T[A])(f: A => M[B])(implicit P: Parallel[M, F]): M[Unit] = {
    implicit val F = P.applicative
    implicit val M = P.monad
    val gtb: F[Unit] = Foldable[T].traverse_(ta)(f andThen P.parallel.apply)
    P.sequential(gtb)
  }

  /**
    * Like `Applicative[F].ap`, but uses the applicative instance
    * corresponding to the Parallel instance instead.
    */
  def parAp[M[_], F[_], A, B](mf: M[A => B])
                                    (ma: M[A])
                                    (implicit P: Parallel[M, F]): M[B] = {
    implicit val F = P.applicative
    implicit val M = P.monad
    val fb = Applicative[F].ap(P.parallel(mf))(P.parallel(ma))
    P.sequential(fb)
  }

  /**
    * Like `Applicative[F].product`, but uses the applicative instance
    * corresponding to the Parallel instance instead.
    */
  def parProduct[M[_]: Monad, F[_], A, B](ma: M[A], mb: M[B])
                                         (implicit P: Parallel[M, F]): M[(A, B)] =
    parAp(Monad[M].map(ma)(a => (b: B) => (a, b)))(mb)

  /**
    * Like `Applicative[F].ap2`, but uses the applicative instance
    * corresponding to the Parallel instance instead.
    */
  def parAp2[M[_]: Monad, F[_], A, B, Z](ff: M[(A, B) => Z])
                                        (ma: M[A], mb: M[B])
                                        (implicit P: Parallel[M, F]): M[Z] = {
    implicit val M = P.monad
    P.sequential(
      P.applicative.ap2(P.parallel(ff))(P.parallel(ma), P.parallel(mb))
    )
  }

  /**
    * Provides an `ApplicativeError[F, E]` instance for any F, that has a `Parallel[M, F]`
    * and a `MonadError[M, E]` instance.
    * I.e. if you have a type M[_], that supports parallel composition through type F[_],
    * then you can get `ApplicativeError[F, E]` from `MonadError[M, E]`.
    */
  def applicativeError[M[_], F[_], E]
  (implicit P: Parallel[M, F], E: MonadError[M, E]): ApplicativeError[F, E] = P.applicativeError

  /**
    * A Parallel instance for any type `M[_]` that supports parallel composition through itself.
    * Can also be used for giving `Parallel` instances to types that do not support parallel composition,
    * but are required to have an instance of `Parallel` defined,
    * in which case parallel composition will actually be sequential.
    */
  def identity[M[_]: Monad]: Parallel[M, M] = new Parallel[M, M] {

    def monad: Monad[M] = implicitly[Monad[M]]

    def applicative: Applicative[M] = implicitly[Monad[M]]

    def sequential: M ~> M = FunctionK.id

    def parallel: M ~> M = FunctionK.id
  }
}
