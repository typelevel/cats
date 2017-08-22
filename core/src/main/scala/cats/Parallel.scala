package cats

trait Parallel[M[_], F[_]] {
  def applicative: Applicative[F]
  def sequential(implicit M: Monad[M]): F ~> M
  def parallel(implicit M: Monad[M]): M ~> F
}

object Parallel extends ParallelArityFunctions {
  def parSequence[T[_]: Traverse, M[_]: Monad, F[_], A]
  (tma: T[M[A]])(implicit P: Parallel[M, F]): M[T[A]] = {
    implicit val F = P.applicative
    val fta: F[T[A]] = Traverse[T].traverse(tma)(P.parallel.apply)
    P.sequential.apply(fta)
  }

  def parTraverse[T[_]: Traverse, M[_]: Monad, F[_], A, B]
  (ta: T[A])(f: A => M[B])(implicit P: Parallel[M, F]): M[T[B]] = {
    implicit val F = P.applicative
    val gtb: F[T[B]] = Traverse[T].traverse(ta)(f andThen P.parallel.apply)
    P.sequential.apply(gtb)
  }

  def parSequence_[T[_]: Foldable, M[_]: Monad, F[_], A]
  (tma: T[M[A]])(implicit P: Parallel[M, F]): M[Unit] = {
    implicit val F = P.applicative
    val fu: F[Unit] = Foldable[T].traverse_(tma)(P.parallel.apply)
    P.sequential.apply(fu)
  }

  def parTraverse_[T[_]: Foldable, M[_]: Monad, F[_], A, B]
  (ta: T[A])(f: A => M[B])(implicit P: Parallel[M, F]): M[Unit] = {
    implicit val F = P.applicative
    val gtb: F[Unit] = Foldable[T].traverse_(ta)(f andThen P.parallel.apply)
    P.sequential.apply(gtb)
  }

  def parAp[M[_]: Monad, F[_], A, B](mf: M[A => B])
                                    (ma: M[A])
                                    (implicit P: Parallel[M, F]): M[B] = {
    implicit val F = P.applicative
    val fb = Applicative[F].ap(P.parallel.apply(mf))(P.parallel.apply(ma))
    P.sequential.apply(fb)
  }

  def parProduct[M[_]: Monad, F[_], A, B](ma: M[A], mb: M[B])
                                         (implicit P: Parallel[M, F]): M[(A, B)] =
    parAp(Monad[M].map(ma)(a => (b: B) => (a, b)))(mb)

  def parAp2[M[_]: Monad, F[_], A, B, Z](ff: M[(A, B) => Z])
                                        (ma: M[A], mb: M[B])
                                        (implicit P: Parallel[M, F]): M[Z] =
    Monad[M].map(parProduct(ma, parProduct(mb, ff))) { case (a, (b, f)) => f(a, b) }
}
