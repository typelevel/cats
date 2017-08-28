package cats

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
    * Natural Transformation from the parallel Applicative F[_] to the sequential Monad M[_].
    */
  def sequential(implicit M: Monad[M]): F ~> M

  /**
    * Natural Transformation from the sequential Monad M[_] to the parallel Applicative F[_].
    */
  def parallel(implicit M: Monad[M]): M ~> F
}

object Parallel extends ParallelArityFunctions {

  def apply[M[_], F[_]](implicit P: Parallel[M, F]): Parallel[M, F] = P

  /**
    * Like `Traverse[A].sequence`, but uses the applicative instance
    * corresponding to the Parallel instance instead.
    */
  def parSequence[T[_]: Traverse, M[_]: Monad, F[_], A]
  (tma: T[M[A]])(implicit P: Parallel[M, F]): M[T[A]] = {
    implicit val F = P.applicative
    val fta: F[T[A]] = Traverse[T].traverse(tma)(P.parallel.apply)
    P.sequential.apply(fta)
  }

  /**
    * Like `Traverse[A].traverse`, but uses the applicative instance
    * corresponding to the Parallel instance instead.
    */
  def parTraverse[T[_]: Traverse, M[_]: Monad, F[_], A, B]
  (ta: T[A])(f: A => M[B])(implicit P: Parallel[M, F]): M[T[B]] = {
    implicit val F = P.applicative
    val gtb: F[T[B]] = Traverse[T].traverse(ta)(f andThen P.parallel.apply)
    P.sequential.apply(gtb)
  }

  /**
    * Like `Foldable[A].sequence_`, but uses the applicative instance
    * corresponding to the Parallel instance instead.
    */
  def parSequence_[T[_]: Foldable, M[_]: Monad, F[_], A]
  (tma: T[M[A]])(implicit P: Parallel[M, F]): M[Unit] = {
    implicit val F = P.applicative
    val fu: F[Unit] = Foldable[T].traverse_(tma)(P.parallel.apply)
    P.sequential.apply(fu)
  }

  /**
    * Like `Foldable[A].traverse_`, but uses the applicative instance
    * corresponding to the Parallel instance instead.
    */
  def parTraverse_[T[_]: Foldable, M[_]: Monad, F[_], A, B]
  (ta: T[A])(f: A => M[B])(implicit P: Parallel[M, F]): M[Unit] = {
    implicit val F = P.applicative
    val gtb: F[Unit] = Foldable[T].traverse_(ta)(f andThen P.parallel.apply)
    P.sequential.apply(gtb)
  }

  /**
    * Like `Applicative[F].ap`, but uses the applicative instance
    * corresponding to the Parallel instance instead.
    */
  def parAp[M[_]: Monad, F[_], A, B](mf: M[A => B])
                                    (ma: M[A])
                                    (implicit P: Parallel[M, F]): M[B] = {
    implicit val F = P.applicative
    val fb = Applicative[F].ap(P.parallel.apply(mf))(P.parallel.apply(ma))
    P.sequential.apply(fb)
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
                                        (implicit P: Parallel[M, F]): M[Z] =
    Monad[M].map(parProduct(ma, parProduct(mb, ff))) { case (a, (b, f)) => f(a, b) }
}
