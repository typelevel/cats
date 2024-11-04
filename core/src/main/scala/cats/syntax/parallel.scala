/*
 * Copyright (c) 2015 Typelevel
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package cats.syntax

import cats.{
  Bitraverse,
  CommutativeApplicative,
  FlatMap,
  Foldable,
  Monad,
  Monoid,
  NonEmptyParallel,
  Parallel,
  Reducible,
  Semigroup,
  Traverse,
  TraverseFilter,
  UnorderedTraverse
}

trait ParallelSyntax extends TupleParallelSyntax {

  @deprecated("Kept for binary compatibility", "2.6.0")
  final def catsSyntaxParallelTraverse[T[_]: Traverse, A](ta: T[A]): ParallelTraversableOps[T, A] =
    new ParallelTraversableOps(ta)

  implicit final def catsSyntaxParallelTraverse1[T[_]: Traverse, A](ta: T[A]): ParallelTraversableOps1[T, A] =
    new ParallelTraversableOps1(ta)

  @deprecated("Kept for binary compatibility", "2.6.0")
  final def catsSyntaxParallelSequence[T[_]: Traverse, M[_]: Monad, A](tma: T[M[A]]): ParallelSequenceOps[T, M, A] =
    new ParallelSequenceOps(tma)

  implicit final def catsSyntaxParallelSequence1[T[_]: Traverse, M[_], A](tma: T[M[A]]): ParallelSequenceOps1[T, M, A] =
    new ParallelSequenceOps1(tma)

  @deprecated("Kept for binary compatibility", "2.8.0")
  final def catsSyntaxParallelAp[M[_]: FlatMap, A](ma: M[A]): ParallelApOps[M, A] =
    new ParallelApOps(ma)

  implicit final def catsSyntaxParallelAp1[M[_], A](ma: M[A]): ParallelApOps1[M, A] =
    new ParallelApOps1(ma)

  implicit final def catsSyntaxNonEmptyParallelAp[M[_], A](ma: M[A]): NonEmptyParallelApOps[M, A] =
    new NonEmptyParallelApOps(ma)
}

trait ParallelApplySyntax {
  @deprecated("Kept for binary compatibility", "2.8.0")
  final def catsSyntaxParallelApply[F[_], A, B](fa: F[A => B]): ParallelApplyOps[F, A, B] =
    new ParallelApplyOps(fa)

  implicit final def catsSyntaxNonEmptyParallelApply[F[_], A, B](fa: F[A => B]): NonEmptyParallelApplyOps[F, A, B] =
    new NonEmptyParallelApplyOps(fa)
}

trait ParallelFlatSyntax {
  @deprecated("Kept for binary compatibility", "2.6.0")
  final def catsSyntaxParallelFlatTraverse[T[_]: Traverse: FlatMap, A](ta: T[A]): ParallelFlatTraversableOps[T, A] =
    new ParallelFlatTraversableOps(ta)

  implicit final def catsSyntaxParallelFlatTraverse1[T[_]: Traverse: FlatMap, A](
    ta: T[A]
  ): ParallelFlatTraversableOps1[T, A] =
    new ParallelFlatTraversableOps1(ta)

  @deprecated("Kept for binary compatibility", "2.6.0")
  final def catsSyntaxParallelFlatSequence[T[_]: Traverse: FlatMap, M[_]: Monad, A](
    tmta: T[M[T[A]]]
  ): ParallelFlatSequenceOps[T, M, A] =
    new ParallelFlatSequenceOps(tmta)

  implicit final def catsSyntaxParallelFlatSequence1[T[_]: Traverse: FlatMap, M[_], A](
    tmta: T[M[T[A]]]
  ): ParallelFlatSequenceOps1[T, M, A] =
    new ParallelFlatSequenceOps1(tmta)
}

trait ParallelTraverseFilterSyntax {
  implicit final def catsSyntaxParallelTraverseFilter[T[_]: TraverseFilter, A](
    ta: T[A]
  ): ParallelTraverseFilterOps[T, A] =
    new ParallelTraverseFilterOps(ta)

  implicit final def catsSyntaxParallelSequenceFilter[T[_]: TraverseFilter, M[_]: Parallel, A](
    tmoa: T[M[Option[A]]]
  ): ParallelSequenceFilterOps[T, M, A] =
    new ParallelSequenceFilterOps(tmoa)
}

trait ParallelTraverseSyntax {
  // Note: this could be renamed to `catsSyntaxParallelTraverseVoid` for consistency,
  // but it looks like too much of a hassle of fighting binary compatibility issues for the implicit part of the API.
  implicit final def catsSyntaxParallelTraverse_[T[_]: Foldable, A](ta: T[A]): ParallelTraversable_Ops[T, A] =
    new ParallelTraversable_Ops(ta)

  // Note: this could be renamed to `catsSyntaxParallelSequenceVoid` for consistency,
  // but it looks like too much of a hassle of fighting binary compatibility issues for the implicit part of the API.
  implicit final def catsSyntaxParallelSequence_[T[_]: Foldable, M[_], A](tma: T[M[A]]): ParallelSequence_Ops[T, M, A] =
    new ParallelSequence_Ops(tma)
}

trait ParallelBitraverseSyntax {
  implicit final def catsSyntaxParallelBitraverse[T[_, _]: Bitraverse, A, B](
    tab: T[A, B]
  ): ParallelBitraverseOps[T, A, B] =
    new ParallelBitraverseOps(tab)

  implicit final def catsSyntaxParallelBisequence[T[_, _]: Bitraverse, M[_], A, B](
    tmamb: T[M[A], M[B]]
  ): ParallelBisequenceOps[T, M, A, B] =
    new ParallelBisequenceOps(tmamb)

  implicit final def catsSyntaxParallelLeftTraverse[T[_, _]: Bitraverse, A, B](
    tab: T[A, B]
  ): ParallelLeftTraverseOps[T, A, B] =
    new ParallelLeftTraverseOps(tab)

  implicit final def catsSyntaxParallelLeftSequence[T[_, _]: Bitraverse, M[_], A, B](
    tmab: T[M[A], B]
  ): ParallelLeftSequenceOps[T, M, A, B] =
    new ParallelLeftSequenceOps(tmab)
}

trait ParallelUnorderedTraverseSyntax {
  implicit final def catsSyntaxParallelUnorderedTraverse[T[_], A](
    ta: T[A]
  ): ParallelUnorderedTraverseOps[T, A] =
    new ParallelUnorderedTraverseOps(ta)

  implicit final def catsSyntaxParallelUnorderedSequence[T[_], M[_], A](
    tma: T[M[A]]
  ): ParallelUnorderedSequenceOps[T, M, A] =
    new ParallelUnorderedSequenceOps(tma)

  implicit final def catsSyntaxParallelUnorderedFlatSequence[T[_], M[_], A](
    tmta: T[M[T[A]]]
  ): ParallelUnorderedFlatSequenceOps[T, M, A] =
    new ParallelUnorderedFlatSequenceOps(tmta)
}

trait ParallelFoldMapASyntax {
  implicit final def catsSyntaxParallelFoldMapA[T[_], A](ta: T[A]): ParallelFoldMapAOps[T, A] =
    new ParallelFoldMapAOps(ta)
}

trait ParallelReduceMapASyntax {
  implicit final def catsSyntaxParallelReduceMapA[T[_], A](ta: T[A]): ParallelReduceMapAOps[T, A] =
    new ParallelReduceMapAOps(ta)
}

@deprecated("Kept for binary compatibility", "2.6.0")
final class ParallelTraversableOps[T[_], A](private val ta: T[A]) extends AnyVal {
  def parTraverse[M[_]: Monad, B](f: A => M[B])(implicit T: Traverse[T], P: Parallel[M]): M[T[B]] =
    Parallel.parTraverse(ta)(f)
}

final class ParallelTraversableOps1[T[_], A](private val ta: T[A]) extends AnyVal {
  def parTraverse[M[_], B](f: A => M[B])(implicit T: Traverse[T], P: Parallel[M]): M[T[B]] =
    Parallel.parTraverse(ta)(f)
}

final class ParallelTraverseFilterOps[T[_], A](private val ta: T[A]) extends AnyVal {
  def parTraverseFilter[M[_]: Parallel, B](f: A => M[Option[B]])(implicit T: TraverseFilter[T]): M[T[B]] =
    Parallel.parTraverseFilter(ta)(f)

  def parFilterA[M[_]: Parallel](f: A => M[Boolean])(implicit T: TraverseFilter[T]): M[T[A]] =
    Parallel.parFilterA(ta)(f)
}

final class ParallelSequenceFilterOps[T[_], M[_], A](private val tmoa: T[M[Option[A]]]) extends AnyVal {
  def parSequenceFilter(implicit P: Parallel[M], T: TraverseFilter[T]): M[T[A]] =
    Parallel.parSequenceFilter(tmoa)
}

// Note: this could be renamed to `ParallelTraversableVoidOps` for consistency,
// but it looks like too much of a hassle of fighting binary compatibility issues for the implicit part of the API.
final class ParallelTraversable_Ops[T[_], A](private val ta: T[A]) extends AnyVal {
  def parTraverseVoid[M[_], B](f: A => M[B])(implicit T: Foldable[T], P: Parallel[M]): M[Unit] =
    Parallel.parTraverseVoid(ta)(f)
  def parTraverse_[M[_], B](f: A => M[B])(implicit T: Foldable[T], P: Parallel[M]): M[Unit] =
    parTraverseVoid(f)
}

@deprecated("Kept for binary compatibility", "2.6.0")
final class ParallelFlatTraversableOps[T[_], A](private val ta: T[A]) extends AnyVal {
  def parFlatTraverse[M[_]: Monad, B](
    f: A => M[T[B]]
  )(implicit T0: Traverse[T], T1: FlatMap[T], P: Parallel[M]): M[T[B]] =
    Parallel.parFlatTraverse(ta)(f)
}

final class ParallelFlatTraversableOps1[T[_], A](private val ta: T[A]) extends AnyVal {
  def parFlatTraverse[M[_], B](f: A => M[T[B]])(implicit T0: Traverse[T], T1: FlatMap[T], P: Parallel[M]): M[T[B]] =
    Parallel.parFlatTraverse(ta)(f)
}

@deprecated("Kept for binary compatibility", "2.6.0")
final class ParallelSequenceOps[T[_], M[_], A](private val tma: T[M[A]]) extends AnyVal {
  def parSequence(implicit M: Monad[M], T: Traverse[T], P: Parallel[M]): M[T[A]] =
    Parallel.parSequence(tma)
}

final class ParallelSequenceOps1[T[_], M[_], A](private val tma: T[M[A]]) extends AnyVal {
  def parSequence(implicit T: Traverse[T], P: Parallel[M]): M[T[A]] =
    Parallel.parSequence(tma)
}

// Note: this could be renamed to `ParallelSequenceVoidOps` for consistency,
// but it looks like too much of a hassle of fighting binary compatibility issues for the implicit part of the API.
final class ParallelSequence_Ops[T[_], M[_], A](private val tma: T[M[A]]) extends AnyVal {
  def parSequenceVoid(implicit T: Foldable[T], P: Parallel[M]): M[Unit] =
    Parallel.parSequenceVoid(tma)
  def parSequence_(implicit T: Foldable[T], P: Parallel[M]): M[Unit] =
    parSequenceVoid
}

@deprecated("Kept for binary compatibility", "2.6.0")
final class ParallelFlatSequenceOps[T[_], M[_], A](private val tmta: T[M[T[A]]]) extends AnyVal {
  def parFlatSequence(implicit M: Monad[M], T0: Traverse[T], T1: FlatMap[T], P: Parallel[M]): M[T[A]] =
    Parallel.parFlatSequence(tmta)
}

final class ParallelFlatSequenceOps1[T[_], M[_], A](private val tmta: T[M[T[A]]]) extends AnyVal {
  def parFlatSequence(implicit T0: Traverse[T], T1: FlatMap[T], P: Parallel[M]): M[T[A]] =
    Parallel.parFlatSequence(tmta)
}

final class ParallelUnorderedSequenceOps[T[_], M[_], A](private val tmta: T[M[A]]) extends AnyVal {
  def parUnorderedSequence[F[_]](implicit
    P: Parallel.Aux[M, F],
    F: CommutativeApplicative[F],
    Tutraverse: UnorderedTraverse[T]
  ): M[T[A]] =
    Parallel.parUnorderedSequence(tmta)
}

final class ParallelUnorderedTraverseOps[T[_], A](private val ta: T[A]) extends AnyVal {
  def parUnorderedTraverse[M[_], F[_], B](
    f: A => M[B]
  )(implicit P: Parallel.Aux[M, F], F: CommutativeApplicative[F], Tutraverse: UnorderedTraverse[T]): M[T[B]] =
    Parallel.parUnorderedTraverse(ta)(f)

  def parUnorderedFlatTraverse[M[_], F[_], B](
    f: A => M[T[B]]
  )(implicit
    P: Parallel.Aux[M, F],
    F: CommutativeApplicative[F],
    Tflatmap: FlatMap[T],
    Tutraverse: UnorderedTraverse[T]
  ): M[T[B]] =
    Parallel.parUnorderedFlatTraverse(ta)(f)
}

final class ParallelUnorderedFlatSequenceOps[T[_], M[_], A](private val tmta: T[M[T[A]]]) extends AnyVal {
  def parUnorderedFlatSequence[F[_]](implicit
    P: Parallel.Aux[M, F],
    Tflatmap: FlatMap[T],
    F: CommutativeApplicative[F],
    Tutraverse: UnorderedTraverse[T]
  ): M[T[A]] =
    Parallel.parUnorderedFlatSequence(tmta)
}

@deprecated("Kept for binary compatibility", "2.8.0")
final class ParallelApOps[M[_], A](private val ma: M[A]) extends AnyVal {
  def &>[B](mb: M[B])(implicit P: Parallel[M]): M[B] =
    P.parProductR(ma)(mb)

  def <&[B](mb: M[B])(implicit P: Parallel[M]): M[A] =
    P.parProductL(ma)(mb)

  def parProductL[B](mb: M[B])(implicit P: Parallel[M]): M[A] =
    P.parProductL(ma)(mb)

  def parProductR[B](mb: M[B])(implicit P: Parallel[M]): M[B] =
    P.parProductR(ma)(mb)

  def parProduct[B](mb: M[B])(implicit P: Parallel[M]): M[(A, B)] =
    Parallel.parProduct(ma, mb)

  def parReplicateA(n: Int)(implicit P: Parallel[M]): M[List[A]] =
    Parallel.parReplicateA(n, ma)

  def parReplicateA_(n: Int)(implicit P: Parallel[M]): M[Unit] =
    Parallel.parReplicateA_(n, ma)
}

final class ParallelApOps1[M[_], A](private val ma: M[A]) extends AnyVal {
  def parReplicateA(n: Int)(implicit P: Parallel[M]): M[List[A]] =
    Parallel.parReplicateA(n, ma)
  def parReplicateA_(n: Int)(implicit P: Parallel[M]): M[Unit] =
    Parallel.parReplicateA_(n, ma)
}

final class NonEmptyParallelApOps[M[_], A](private val ma: M[A]) extends AnyVal {
  def &>[B](mb: M[B])(implicit P: NonEmptyParallel[M]): M[B] =
    P.parProductR[A, B](ma)(mb)

  def <&[B](mb: M[B])(implicit P: NonEmptyParallel[M]): M[A] =
    P.parProductL[A, B](ma)(mb)

  def parProductL[B](mb: M[B])(implicit P: NonEmptyParallel[M]): M[A] =
    P.parProductL[A, B](ma)(mb)

  def parProductR[B](mb: M[B])(implicit P: NonEmptyParallel[M]): M[B] =
    P.parProductR[A, B](ma)(mb)

  def parProduct[B](mb: M[B])(implicit P: NonEmptyParallel[M]): M[(A, B)] =
    Parallel.parProduct(ma, mb)
}

@deprecated("Kept for binary compatibility", "2.8.0")
final class ParallelApplyOps[M[_], A, B](private val mab: M[A => B]) extends AnyVal {
  def <&>(ma: M[A])(implicit P: Parallel[M]): M[B] =
    Parallel.parAp(mab)(ma)(P)

  def parAp(ma: M[A])(implicit P: Parallel[M]): M[B] =
    Parallel.parAp(mab)(ma)
}

final class NonEmptyParallelApplyOps[M[_], A, B](private val mab: M[A => B]) extends AnyVal {
  def <&>(ma: M[A])(implicit P: NonEmptyParallel[M]): M[B] =
    Parallel.parAp[M, A, B](mab)(ma)(P)

  def parAp(ma: M[A])(implicit P: NonEmptyParallel[M]): M[B] =
    Parallel.parAp[M, A, B](mab)(ma)
}

final class ParallelBitraverseOps[T[_, _], A, B](private val tab: T[A, B]) extends AnyVal {
  def parBitraverse[M[_], C, D](f: A => M[C], g: B => M[D])(implicit T: Bitraverse[T], P: Parallel[M]): M[T[C, D]] =
    Parallel.parBitraverse(tab)(f, g)
}

final class ParallelBisequenceOps[T[_, _], M[_], A, B](private val tmamb: T[M[A], M[B]]) extends AnyVal {
  def parBisequence(implicit T: Bitraverse[T], P: Parallel[M]): M[T[A, B]] =
    Parallel.parBisequence(tmamb)
}

final class ParallelLeftTraverseOps[T[_, _], A, B](private val tab: T[A, B]) extends AnyVal {
  def parLeftTraverse[M[_], C](f: A => M[C])(implicit T: Bitraverse[T], P: Parallel[M]): M[T[C, B]] =
    Parallel.parLeftTraverse(tab)(f)
}

final class ParallelLeftSequenceOps[T[_, _], M[_], A, B](private val tmab: T[M[A], B]) extends AnyVal {
  def parLeftSequence(implicit T: Bitraverse[T], P: Parallel[M]): M[T[A, B]] =
    Parallel.parLeftSequence(tmab)
}

final class ParallelFoldMapAOps[T[_], A](private val ma: T[A]) extends AnyVal {
  def parFoldMapA[M[_], B](f: A => M[B])(implicit T: Foldable[T], P: Parallel[M], B: Monoid[B]): M[B] =
    Parallel.parFoldMapA(ma)(f)
}

final class ParallelReduceMapAOps[T[_], A](private val ma: T[A]) extends AnyVal {
  def parReduceMapA[M[_], B](f: A => M[B])(implicit T: Reducible[T], P: NonEmptyParallel[M], B: Semigroup[B]): M[B] =
    Parallel.parReduceMapA(ma)(f)
}
