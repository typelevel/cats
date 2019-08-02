package cats.syntax

import cats.{Bitraverse, CommutativeApplicative, FlatMap, Foldable, Monad, Parallel, Traverse, UnorderedTraverse}

trait ParallelSyntax extends TupleParallelSyntax {

  implicit final def catsSyntaxParallelTraverse[T[_]: Traverse, A](ta: T[A]): ParallelTraversableOps[T, A] =
    new ParallelTraversableOps[T, A](ta)

  implicit final def catsSyntaxParallelSequence[T[_]: Traverse, M[_]: Monad, A](
    tma: T[M[A]]
  ): ParallelSequenceOps[T, M, A] = new ParallelSequenceOps[T, M, A](tma)

  implicit final def catsSyntaxParallelAp[M[_]: FlatMap, A](ma: M[A]): ParallelApOps[M, A] =
    new ParallelApOps[M, A](ma)

}

trait ParallelApplySyntax {
  implicit final def catsSyntaxParallelApply[F[_], A, B](fa: F[A => B]): ParallelApplyOps[F, A, B] =
    new ParallelApplyOps[F, A, B](fa)
}

trait ParallelFlatSyntax {
  implicit final def catsSyntaxParallelFlatTraverse[T[_]: Traverse: FlatMap, A](
    ta: T[A]
  ): ParallelFlatTraversableOps[T, A] = new ParallelFlatTraversableOps[T, A](ta)

  implicit final def catsSyntaxParallelFlatSequence[T[_]: Traverse: FlatMap, M[_]: Monad, A](
    tmta: T[M[T[A]]]
  ): ParallelFlatSequenceOps[T, M, A] = new ParallelFlatSequenceOps[T, M, A](tmta)
}

trait ParallelTraverseSyntax {
  implicit final def catsSyntaxParallelTraverse_[T[_]: Foldable, A](ta: T[A]): ParallelTraversable_Ops[T, A] =
    new ParallelTraversable_Ops[T, A](ta)

  implicit final def catsSyntaxParallelSequence_[T[_]: Foldable, M[_], A](tma: T[M[A]]): ParallelSequence_Ops[T, M, A] =
    new ParallelSequence_Ops[T, M, A](tma)
}

trait ParallelBitraverseSyntax {
  implicit final def catsSyntaxParallelBitraverse[T[_, _]: Bitraverse, A, B](
    tab: T[A, B]
  ): ParallelBitraverseOps[T, A, B] =
    new ParallelBitraverseOps[T, A, B](tab)

  implicit final def catsSyntaxParallelBisequence[T[_, _]: Bitraverse, M[_], A, B](
    tmamb: T[M[A], M[B]]
  ): ParallelBisequenceOps[T, M, A, B] =
    new ParallelBisequenceOps[T, M, A, B](tmamb)

  implicit final def catsSyntaxParallelLeftTraverse[T[_, _]: Bitraverse, A, B](
    tab: T[A, B]
  ): ParallelLeftTraverseOps[T, A, B] =
    new ParallelLeftTraverseOps[T, A, B](tab)

  implicit final def catsSyntaxParallelLeftSequence[T[_, _]: Bitraverse, M[_], A, B](
    tmab: T[M[A], B]
  ): ParallelLeftSequenceOps[T, M, A, B] =
    new ParallelLeftSequenceOps[T, M, A, B](tmab)
}

trait ParallelUnorderedTraverseSyntax {
  implicit final def catsSyntaxParallelUnorderedTraverse[T[_], A](
    ta: T[A]
  ): ParallelUnorderedTraverseOps[T, A] =
    new ParallelUnorderedTraverseOps[T, A](ta)

  implicit final def catsSyntaxParallelUnorderedSequence[T[_], M[_], A](
    tma: T[M[A]]
  ): ParallelUnorderedSequenceOps[T, M, A] =
    new ParallelUnorderedSequenceOps[T, M, A](tma)

  implicit final def catsSyntaxParallelUnorderedFlatTraverse[T[_], A](
    ta: T[A]
  ): ParallelUnorderedFlatTraverseOps[T, A] =
    new ParallelUnorderedFlatTraverseOps[T, A](ta)

  implicit final def catsSyntaxParallelUnorderedFlatSequence[T[_], M[_], A](
    tmta: T[M[T[A]]]
  ): ParallelUnorderedFlatSequenceOps[T, M, A] =
    new ParallelUnorderedFlatSequenceOps[T, M, A](tmta)

}

final class ParallelTraversableOps[T[_], A](private val ta: T[A]) extends AnyVal {
  def parTraverse[M[_]: Monad, F[_], B](f: A => M[B])(implicit T: Traverse[T], P: Parallel[M, F]): M[T[B]] =
    Parallel.parTraverse(ta)(f)

}

final class ParallelTraversable_Ops[T[_], A](private val ta: T[A]) extends AnyVal {
  def parTraverse_[M[_], F[_], B](f: A => M[B])(implicit T: Foldable[T], P: Parallel[M, F]): M[Unit] =
    Parallel.parTraverse_(ta)(f)
}

final class ParallelFlatTraversableOps[T[_], A](private val ta: T[A]) extends AnyVal {
  def parFlatTraverse[M[_]: Monad, F[_], B](
    f: A => M[T[B]]
  )(implicit T0: Traverse[T], T1: FlatMap[T], P: Parallel[M, F]): M[T[B]] =
    Parallel.parFlatTraverse(ta)(f)
}

final class ParallelSequenceOps[T[_], M[_], A](private val tma: T[M[A]]) extends AnyVal {
  def parSequence[F[_]](implicit M: Monad[M], T: Traverse[T], P: Parallel[M, F]): M[T[A]] =
    Parallel.parSequence(tma)
}

final class ParallelSequence_Ops[T[_], M[_], A](private val tma: T[M[A]]) extends AnyVal {
  def parSequence_[F[_]](implicit T: Foldable[T], P: Parallel[M, F]): M[Unit] =
    Parallel.parSequence_(tma)
}

final class ParallelFlatSequenceOps[T[_], M[_], A](private val tmta: T[M[T[A]]]) extends AnyVal {
  def parFlatSequence[F[_]](implicit M: Monad[M], T0: Traverse[T], T1: FlatMap[T], P: Parallel[M, F]): M[T[A]] =
    Parallel.parFlatSequence(tmta)
}

final class ParallelUnorderedSequenceOps[T[_], M[_], A](private val tmta: T[M[A]]) extends AnyVal {
  def parUnorderedSequence[F[_]](implicit P: Parallel[M, F],
                                 F: CommutativeApplicative[F],
                                 Tutraverse: UnorderedTraverse[T]): M[T[A]] =
    Parallel.parUnorderedSequence(tmta)
}

final class ParallelUnorderedTraverseOps[T[_], A](private val ta: T[A]) extends AnyVal {
  def parUnorderedTraverse[M[_], F[_], B](
    f: A => M[B]
  )(implicit P: Parallel[M, F], F: CommutativeApplicative[F], Tutraverse: UnorderedTraverse[T]): M[T[B]] =
    Parallel.parUnorderedTraverse(ta)(f)
}

final class ParallelUnorderedFlatSequenceOps[T[_], M[_], A](private val tmta: T[M[T[A]]]) extends AnyVal {
  def parUnorderedFlatSequence[F[_]](implicit P: Parallel[M, F],
                                     Tflatmap: FlatMap[T],
                                     F: CommutativeApplicative[F],
                                     Tutraverse: UnorderedTraverse[T]): M[T[A]] =
    Parallel.parUnorderedFlatSequence(tmta)
}

final class ParallelUnorderedFlatTraverseOps[T[_], A](private val ta: T[A]) extends AnyVal {
  def parUnorderedFlatTraverse[M[_], F[_], B](
    f: A => M[T[B]]
  )(implicit P: Parallel[M, F],
    F: CommutativeApplicative[F],
    Tflatmap: FlatMap[T],
    Tutraverse: UnorderedTraverse[T]): M[T[B]] =
    Parallel.parUnorderedFlatTraverse(ta)(f)
}

final class ParallelApOps[M[_], A](private val ma: M[A]) extends AnyVal {

  def &>[F[_], B](mb: M[B])(implicit P: Parallel[M, F]): M[B] =
    P.parProductR(ma)(mb)

  def <&[F[_], B](mb: M[B])(implicit P: Parallel[M, F]): M[A] =
    P.parProductL(ma)(mb)

}

final class ParallelApplyOps[M[_], A, B](private val mab: M[A => B]) extends AnyVal {
  def <&>[F[_]](ma: M[A])(implicit P: Parallel[M, F]): M[B] =
    Parallel.parAp(mab)(ma)
}

final class ParallelBitraverseOps[T[_, _], A, B](private val tab: T[A, B]) extends AnyVal {
  def parBitraverse[M[_], F[_], C, D](f: A => M[C], g: B => M[D])(implicit T: Bitraverse[T],
                                                                  P: Parallel[M, F]): M[T[C, D]] =
    Parallel.parBitraverse(tab)(f, g)
}

final class ParallelBisequenceOps[T[_, _], M[_], A, B](private val tmamb: T[M[A], M[B]]) extends AnyVal {
  def parBisequence[F[_]](implicit T: Bitraverse[T], P: Parallel[M, F]): M[T[A, B]] =
    Parallel.parBisequence(tmamb)
}

final class ParallelLeftTraverseOps[T[_, _], A, B](private val tab: T[A, B]) extends AnyVal {
  def parLeftTraverse[M[_], F[_], C](f: A => M[C])(implicit T: Bitraverse[T], P: Parallel[M, F]): M[T[C, B]] =
    Parallel.parLeftTraverse(tab)(f)
}

final class ParallelLeftSequenceOps[T[_, _], M[_], A, B](private val tmab: T[M[A], B]) extends AnyVal {
  def parLeftSequence[F[_]](implicit T: Bitraverse[T], P: Parallel[M, F]): M[T[A, B]] =
    Parallel.parLeftSequence(tmab)
}
