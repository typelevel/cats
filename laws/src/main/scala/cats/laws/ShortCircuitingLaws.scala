package cats.laws

import java.util.concurrent.atomic.AtomicLong

import cats.instances.option._
import cats.syntax.foldable._
import cats.syntax.traverse._
import cats.syntax.nonEmptyTraverse._
import cats.syntax.traverseFilter._
import cats.{Applicative, Foldable, MonoidK, NonEmptyTraverse, Traverse, TraverseFilter}

trait ShortCircuitingLaws[F[_]] {

  def foldMapKShortCircuits[A](fa: F[A], empty: A)(implicit F: Foldable[F]): IsEq[Long] = {
    val size = fa.size
    val maxInvocationsAllowed = size / 2
    val f = new RestrictedFunction[A, Option[A]]((a: A) => None, maxInvocationsAllowed, Some(empty))

    fa.foldMapK(f)
    f.invocations.get <-> (maxInvocationsAllowed + 1).min(size)
  }

  def foldMapKWontShortCircuit[A](fa: F[A], empty: A)(implicit F: Foldable[F]): IsEq[Long] = {
    val size = fa.size
    val maxInvocationsAllowed = size / 2
    val f = new RestrictedFunction[A, Option[A]]((a: A) => None, maxInvocationsAllowed, Some(empty))

    fa.foldMapK(f)(F, nonShortCircuitingMonoidK)
    f.invocations.get <-> size
  }

  def traverseShortCircuits[A](fa: F[A])(implicit F: Traverse[F]): IsEq[Long] = {
    val size = fa.size
    val maxInvocationsAllowed = size / 2
    val f = new RestrictedFunction((i: A) => Some(i), maxInvocationsAllowed, None)

    fa.traverse(f)
    f.invocations.get <-> (maxInvocationsAllowed + 1).min(size)
  }

  def traverseWontShortCircuit[A](fa: F[A])(implicit F: Traverse[F]): IsEq[Long] = {
    val size = fa.size
    val maxInvocationsAllowed = size / 2
    val f = new RestrictedFunction((i: A) => Some(i), maxInvocationsAllowed, None)

    fa.traverse(f)(nonShortCircuitingApplicative)
    f.invocations.get <-> size
  }

  def nonEmptyTraverseShortCircuits[A](fa: F[A])(implicit F: NonEmptyTraverse[F]): IsEq[Long] = {
    val size = fa.size
    val maxInvocationsAllowed = size / 2
    val f = new RestrictedFunction((i: A) => Some(i), maxInvocationsAllowed, None)

    fa.nonEmptyTraverse(f)
    f.invocations.get <-> (maxInvocationsAllowed + 1).min(size)
  }

  def nonEmptyTraverseWontShortCircuit[A](fa: F[A])(implicit F: NonEmptyTraverse[F]): IsEq[Long] = {
    val size = fa.size
    val maxInvocationsAllowed = size / 2
    val f = new RestrictedFunction((i: A) => Some(i), maxInvocationsAllowed, None)

    fa.nonEmptyTraverse(f)(nonShortCircuitingApplicative)
    f.invocations.get <-> size
  }

  def traverseFilterShortCircuits[A](fa: F[A])(implicit TF: TraverseFilter[F]): IsEq[Long] = {
    implicit val F: Traverse[F] = TF.traverse

    val size = fa.size
    val maxInvocationsAllowed = size / 2
    val f = new RestrictedFunction((i: A) => Option(Option(i)), maxInvocationsAllowed, None)

    fa.traverseFilter(f)
    f.invocations.get <-> (maxInvocationsAllowed + 1).min(size)
  }

  def traverseFilterWontShortCircuit[A](fa: F[A])(implicit TF: TraverseFilter[F]): IsEq[Long] = {
    implicit val F: Traverse[F] = TF.traverse

    val size = fa.size
    val maxInvocationsAllowed = size / 2
    val f = new RestrictedFunction((i: A) => Option(Option(i)), maxInvocationsAllowed, None)

    fa.traverseFilter(f)(nonShortCircuitingApplicative)
    f.invocations.get <-> size
  }

  def filterAShortCircuits[A](fa: F[A])(implicit TF: TraverseFilter[F]): IsEq[Long] = {
    implicit val F: Traverse[F] = TF.traverse

    val size = fa.size
    val maxInvocationsAllowed = size / 2
    val f = new RestrictedFunction((_: A) => Some(true), maxInvocationsAllowed, None)

    fa.filterA(f)
    f.invocations.get <-> (maxInvocationsAllowed + 1).min(size)
  }

  def filterAWontShortCircuit[A](fa: F[A])(implicit TF: TraverseFilter[F]): IsEq[Long] = {
    implicit val F: Traverse[F] = TF.traverse

    val size = fa.size
    val maxInvocationsAllowed = size / 2
    val f = new RestrictedFunction((_: A) => Some(true), maxInvocationsAllowed, None)

    fa.filterA(f)(nonShortCircuitingApplicative)
    f.invocations.get <-> size
  }

  private[this] class RestrictedFunction[-A, +B](f: A => B, maxInvocationsAllowed: Long, empty: => B) extends (A => B) {
    val invocations = new AtomicLong(0)

    override def apply(v1: A): B =
      if (invocations.getAndIncrement < maxInvocationsAllowed)
        f(v1)
      else
        empty
  }

  private[this] val nonShortCircuitingApplicative: Applicative[Option] = new Applicative[Option] {
    override def pure[A](a: A): Option[A] = Some(a)
    override def ap[A, B](ff: Option[A => B])(fa: Option[A]): Option[B] = ff.flatMap(f => fa.map(f))
  }

  private[this] val nonShortCircuitingMonoidK: MonoidK[Option] = new MonoidK[Option] {
    def empty[A]: Option[A] = None
    def combineK[A](x: Option[A], y: Option[A]): Option[A] = x.orElse(y)
  }
}

object ShortCircuitingLaws {
  def apply[F[_]]: ShortCircuitingLaws[F] = new ShortCircuitingLaws[F] {}
}
