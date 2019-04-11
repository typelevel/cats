package cats

import cats.arrow.FunctionK

/**
 * Some types that form a FlatMap, are also capable of forming an Apply that supports parallel composition.
 * The NonEmptyParallel type class allows us to represent this relationship.
 */
trait NonEmptyParallel[M[_], F[_]] extends Serializable {

  /**
   * The Apply instance for F[_]
   */
  def apply: Apply[F]

  /**
   * The FlatMap instance for M[_]
   */
  def flatMap: FlatMap[M]

  /**
   * Natural Transformation from the parallel Apply F[_] to the sequential FlatMap M[_].
   */
  def sequential: F ~> M

  /**
   * Natural Transformation from the sequential FlatMap M[_] to the parallel Apply F[_].
   */
  def parallel: M ~> F

  /**
   * Like [[Apply.productR]], but uses the apply instance
   * corresponding to the Parallel instance instead.
   */
  def parProductR[A, B](ma: M[A])(mb: M[B]): M[B] =
    Parallel.parMap2(ma, mb)((_, b) => b)(this)

  @deprecated("Use parProductR instead.", "1.0.0-RC2")
  @inline def parFollowedBy[A, B](ma: M[A])(mb: M[B]): M[B] = parProductR(ma)(mb)

  /**
   * Like [[Apply.productL]], but uses the apply instance
   * corresponding to the Parallel instance instead.
   */
  def parProductL[A, B](ma: M[A])(mb: M[B]): M[A] =
    Parallel.parMap2(ma, mb)((a, _) => a)(this)

  @deprecated("Use parProductL instead.", "1.0.0-RC2")
  @inline def parForEffect[A, B](ma: M[A])(mb: M[B]): M[A] = parProductL(ma)(mb)

}

/**
 * Some types that form a Monad, are also capable of forming an Applicative that supports parallel composition.
 * The Parallel type class allows us to represent this relationship.
 */
trait Parallel[M[_], F[_]] extends NonEmptyParallel[M, F] {

  /**
   * The applicative instance for F[_]
   */
  def applicative: Applicative[F]

  /**
   * The monad instance for M[_]
   */
  def monad: Monad[M]

  override def apply: Apply[F] = applicative

  override def flatMap: FlatMap[M] = monad

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
      val ma = MonadError[M, E].handleErrorWith(sequential(fa))(f.andThen(sequential.apply))
      parallel(ma)
    }

    def pure[A](x: A): F[A] = applicative.pure(x)

    def ap[A, B](ff: F[(A) => B])(fa: F[A]): F[B] = applicative.ap(ff)(fa)

    override def map[A, B](fa: F[A])(f: (A) => B): F[B] = applicative.map(fa)(f)

    override def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = applicative.product(fa, fb)

    override def map2[A, B, Z](fa: F[A], fb: F[B])(f: (A, B) => Z): F[Z] = applicative.map2(fa, fb)(f)

    override def map2Eval[A, B, Z](fa: F[A], fb: Eval[F[B]])(f: (A, B) => Z): Eval[F[Z]] =
      applicative.map2Eval(fa, fb)(f)

    override def unlessA[A](cond: Boolean)(f: => F[A]): F[Unit] = applicative.unlessA(cond)(f)

    override def whenA[A](cond: Boolean)(f: => F[A]): F[Unit] = applicative.whenA(cond)(f)
  }
}

object NonEmptyParallel {
  def apply[M[_], F[_]](implicit P: NonEmptyParallel[M, F]): NonEmptyParallel[M, F] = P
}

object Parallel extends ParallelArityFunctions2 {

  def apply[M[_], F[_]](implicit P: Parallel[M, F]): Parallel[M, F] = P

  /**
   * Like `Traverse[A].sequence`, but uses the applicative instance
   * corresponding to the Parallel instance instead.
   */
  def parSequence[T[_]: Traverse, M[_], F[_], A](tma: T[M[A]])(implicit P: Parallel[M, F]): M[T[A]] = {
    val fta: F[T[A]] = Traverse[T].traverse(tma)(P.parallel.apply)(P.applicative)
    P.sequential(fta)
  }

  /**
   * Like `Traverse[A].traverse`, but uses the applicative instance
   * corresponding to the Parallel instance instead.
   */
  def parTraverse[T[_]: Traverse, M[_], F[_], A, B](ta: T[A])(f: A => M[B])(implicit P: Parallel[M, F]): M[T[B]] = {
    val gtb: F[T[B]] = Traverse[T].traverse(ta)(f.andThen(P.parallel.apply))(P.applicative)
    P.sequential(gtb)
  }

  /**
   * Like `Traverse[A].flatTraverse`, but uses the applicative instance
   * corresponding to the Parallel instance instead.
   */
  def parFlatTraverse[T[_]: Traverse: FlatMap, M[_], F[_], A, B](
    ta: T[A]
  )(f: A => M[T[B]])(implicit P: Parallel[M, F]): M[T[B]] = {
    val gtb: F[T[B]] = Traverse[T].flatTraverse(ta)(f.andThen(P.parallel.apply))(P.applicative, FlatMap[T])
    P.sequential(gtb)
  }

  /**
   * Like `Traverse[A].flatSequence`, but uses the applicative instance
   * corresponding to the Parallel instance instead.
   */
  def parFlatSequence[T[_]: Traverse: FlatMap, M[_], F[_], A](tma: T[M[T[A]]])(implicit P: Parallel[M, F]): M[T[A]] = {
    val fta: F[T[A]] = Traverse[T].flatTraverse(tma)(P.parallel.apply)(P.applicative, FlatMap[T])
    P.sequential(fta)
  }

  /**
   * Like `Foldable[A].sequence_`, but uses the applicative instance
   * corresponding to the Parallel instance instead.
   */
  def parSequence_[T[_]: Foldable, M[_], F[_], A](tma: T[M[A]])(implicit P: Parallel[M, F]): M[Unit] = {
    val fu: F[Unit] = Foldable[T].traverse_(tma)(P.parallel.apply)(P.applicative)
    P.sequential(fu)
  }

  /**
   * Like `Foldable[A].traverse_`, but uses the applicative instance
   * corresponding to the Parallel instance instead.
   */
  def parTraverse_[T[_]: Foldable, M[_], F[_], A, B](ta: T[A])(f: A => M[B])(implicit P: Parallel[M, F]): M[Unit] = {
    val gtb: F[Unit] = Foldable[T].traverse_(ta)(f.andThen(P.parallel.apply))(P.applicative)
    P.sequential(gtb)
  }

  /**
   * Like `NonEmptyTraverse[A].nonEmptySequence`, but uses the apply instance
   * corresponding to the Parallel instance instead.
   */
  def parNonEmptySequence[T[_]: NonEmptyTraverse, M[_], F[_], A](
    tma: T[M[A]]
  )(implicit P: NonEmptyParallel[M, F]): M[T[A]] = {
    val fta: F[T[A]] = NonEmptyTraverse[T].nonEmptyTraverse(tma)(P.parallel.apply)(P.apply)
    P.sequential(fta)
  }

  /**
   * Like `NonEmptyTraverse[A].nonEmptyTraverse`, but uses the apply instance
   * corresponding to the Parallel instance instead.
   */
  def parNonEmptyTraverse[T[_]: NonEmptyTraverse, M[_], F[_], A, B](
    ta: T[A]
  )(f: A => M[B])(implicit P: NonEmptyParallel[M, F]): M[T[B]] = {
    val gtb: F[T[B]] = NonEmptyTraverse[T].nonEmptyTraverse(ta)(f.andThen(P.parallel.apply))(P.apply)
    P.sequential(gtb)
  }

  /**
   * Like `NonEmptyTraverse[A].nonEmptyFlatTraverse`, but uses the apply instance
   * corresponding to the Parallel instance instead.
   */
  def parNonEmptyFlatTraverse[T[_]: NonEmptyTraverse: FlatMap, M[_], F[_], A, B](
    ta: T[A]
  )(f: A => M[T[B]])(implicit P: NonEmptyParallel[M, F]): M[T[B]] = {
    val gtb: F[T[B]] = NonEmptyTraverse[T].nonEmptyFlatTraverse(ta)(f.andThen(P.parallel.apply))(P.apply, FlatMap[T])
    P.sequential(gtb)
  }

  /**
   * Like `NonEmptyTraverse[A].nonEmptyFlatSequence`, but uses the apply instance
   * corresponding to the Parallel instance instead.
   */
  def parNonEmptyFlatSequence[T[_]: NonEmptyTraverse: FlatMap, M[_], F[_], A](
    tma: T[M[T[A]]]
  )(implicit P: NonEmptyParallel[M, F]): M[T[A]] = {
    val fta: F[T[A]] = NonEmptyTraverse[T].nonEmptyFlatTraverse(tma)(P.parallel.apply)(P.apply, FlatMap[T])
    P.sequential(fta)
  }

  /**
   * Like `Reducible[A].nonEmptySequence_`, but uses the apply instance
   * corresponding to the Parallel instance instead.
   */
  def parNonEmptySequence_[T[_]: Reducible, M[_], F[_], A](
    tma: T[M[A]]
  )(implicit P: NonEmptyParallel[M, F]): M[Unit] = {
    val fu: F[Unit] = Reducible[T].nonEmptyTraverse_(tma)(P.parallel.apply)(P.apply)
    P.sequential(fu)
  }

  /**
   * Like `Reducible[A].nonEmptyTraverse_`, but uses the apply instance
   * corresponding to the Parallel instance instead.
   */
  def parNonEmptyTraverse_[T[_]: Reducible, M[_], F[_], A, B](
    ta: T[A]
  )(f: A => M[B])(implicit P: NonEmptyParallel[M, F]): M[Unit] = {
    val gtb: F[Unit] = Reducible[T].nonEmptyTraverse_(ta)(f.andThen(P.parallel.apply))(P.apply)
    P.sequential(gtb)
  }

  /**
   * Like `Applicative[F].ap`, but uses the applicative instance
   * corresponding to the Parallel instance instead.
   */
  def parAp[M[_], F[_], A, B](mf: M[A => B])(ma: M[A])(implicit P: NonEmptyParallel[M, F]): M[B] =
    P.sequential(P.apply.ap(P.parallel(mf))(P.parallel(ma)))

  /**
   * Like `Applicative[F].product`, but uses the applicative instance
   * corresponding to the Parallel instance instead.
   */
  def parProduct[M[_], F[_], A, B](ma: M[A], mb: M[B])(implicit P: NonEmptyParallel[M, F]): M[(A, B)] =
    P.sequential(P.apply.product(P.parallel(ma), P.parallel(mb)))

  /**
   * Like `Applicative[F].ap2`, but uses the applicative instance
   * corresponding to the Parallel instance instead.
   */
  def parAp2[M[_], F[_], A, B, Z](ff: M[(A, B) => Z])(ma: M[A], mb: M[B])(implicit P: NonEmptyParallel[M, F]): M[Z] =
    P.sequential(
      P.apply.ap2(P.parallel(ff))(P.parallel(ma), P.parallel(mb))
    )

  /**
   * Provides an `ApplicativeError[F, E]` instance for any F, that has a `Parallel[M, F]`
   * and a `MonadError[M, E]` instance.
   * I.e. if you have a type M[_], that supports parallel composition through type F[_],
   * then you can get `ApplicativeError[F, E]` from `MonadError[M, E]`.
   */
  def applicativeError[M[_], F[_], E](implicit P: Parallel[M, F], E: MonadError[M, E]): ApplicativeError[F, E] =
    P.applicativeError

  /**
   * A Parallel instance for any type `M[_]` that supports parallel composition through itself.
   * Can also be used for giving `Parallel` instances to types that do not support parallel composition,
   * but are required to have an instance of `Parallel` defined,
   * in which case parallel composition will actually be sequential.
   */
  def identity[M[_]: Monad]: Parallel[M, M] = new Parallel[M, M] {

    val monad: Monad[M] = implicitly[Monad[M]]

    val applicative: Applicative[M] = implicitly[Monad[M]]

    val sequential: M ~> M = FunctionK.id

    val parallel: M ~> M = FunctionK.id
  }
}
