package cats.syntax

import cats.{Monad, Parallel, Traverse, FlatMap}

trait ParallelSyntax extends TupleParallelSyntax {
  implicit final def catsSyntaxParallelTraverse[T[_]: Traverse, A]
  (ta: T[A]): ParallelTraversableOps[T, A] = new ParallelTraversableOps[T, A](ta)

  implicit final def catsSyntaxParallelFlatTraverse[T[_]: Traverse: FlatMap, A]
  (ta: T[A]): ParallelFlatTraversableOps[T, A] = new ParallelFlatTraversableOps[T, A](ta)

  implicit final def catsSyntaxParallelSequence[T[_]: Traverse, M[_]: Monad, A]
  (tma: T[M[A]]): ParallelSequenceOps[T, M, A] = new ParallelSequenceOps[T, M, A](tma)

  implicit final def catsSyntaxParallelFlatSequence[T[_]: Traverse: FlatMap, M[_]: Monad, A]
  (tmta: T[M[T[A]]]): ParallelFlatSequenceOps[T, M, A] = new ParallelFlatSequenceOps[T, M, A](tmta)

  implicit final def catsSyntaxParallelAp[M[_]: FlatMap, A](ma: M[A]): ParallelApOps[M, A] =
    new ParallelApOps[M, A](ma)
}


final class ParallelTraversableOps[T[_], A](val ta: T[A]) extends AnyVal {

  def parTraverse[M[_]: Monad, F[_], B]
  (f: A => M[B])(implicit T: Traverse[T], P: Parallel[M, F]): M[T[B]] =
    Parallel.parTraverse(ta)(f)
}

final class ParallelFlatTraversableOps[T[_], A](val ta: T[A]) extends AnyVal {
  def parFlatTraverse[M[_]: Monad, F[_], B]
  (f: A => M[T[B]])(implicit T0: Traverse[T], T1 : FlatMap[T], P: Parallel[M, F]): M[T[B]] =
    Parallel.parFlatTraverse(ta)(f)
}

final class ParallelSequenceOps[T[_], M[_], A](val tma: T[M[A]]) extends AnyVal {
  def parSequence[F[_]]
  (implicit M: Monad[M], T: Traverse[T], P: Parallel[M, F]): M[T[A]] =
    Parallel.parSequence(tma)
}

final class ParallelFlatSequenceOps[T[_], M[_], A](val tmta: T[M[T[A]]]) extends AnyVal {
  def parFlatSequence[F[_]]
  (implicit M: Monad[M], T0: Traverse[T], T1 : FlatMap[T], P: Parallel[M, F]): M[T[A]] =
    Parallel.parFlatSequence(tmta)
}

final class ParallelApOps[M[_], A](val ma: M[A]) extends AnyVal {

  def &>[F[_], B](mb: M[B])(implicit P: Parallel[M, F]): M[B] =
    P.parProductR(ma)(mb)

  def <&[F[_], B](mb: M[B])(implicit P: Parallel[M, F]): M[A] =
    P.parProductL(ma)(mb)

}
