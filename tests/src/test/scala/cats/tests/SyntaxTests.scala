package cats
package tests

import cats.std.AllInstances
import cats.syntax.AllSyntax
import algebra.laws.GroupLaws
import cats.functor.{Invariant, Contravariant}
import cats.laws.discipline.SerializableTests

import org.scalacheck.{Arbitrary}
import org.scalatest.prop.PropertyChecks
import scala.reflect.runtime.universe.TypeTag

/**
 * Test that our syntax implicits are working.
 *
 * Each method should correspond to one type class worth of syntax.
 * Ideally, we should be testing every operator or method that we
 * expect to add to generic parameters. This file is a safeguard
 * against accidentally breaking (or removing) syntax which was
 * otherwise untested.
 *
 * The strategy here is to create "mock" values of particular types,
 * and then ensure that the syntax we want is available. We never plan
 * to run any of these methods, so we don't need real values. All
 * values in the methods should be generic -- we rely on parametricity
 * to guarantee that the syntax will be available for any type with
 * the proper type class instance(s).
 *
 * None of these tests should ever run, or do any runtime checks.
 */
class SyntaxTests extends AllInstances with AllSyntax {

  // pretend we have a value of type A
  def mock[A]: A = ???

  def testSemigroup[A: Semigroup]: Unit = {
    val x = mock[A]
    val y = mock[A]
    val z: A = x |+| y
  }

  def testGroup[A: Group](x: A, y: A): Unit = {
    val x = mock[A]
    val y = mock[A]
    val z: A = x |-| y
  }

  def testEq[A: Eq]: Unit = {
    val x = mock[A]
    val y = mock[A]
    val b0: Boolean = x === y
    val b1: Boolean = x =!= y
  }

  def testPartialOrder[A: PartialOrder]: Unit = {
    val x = mock[A]
    val y = mock[A]
    val b0: Boolean = x < y
    val b1: Boolean = x <= y
    val b2: Boolean = x > y
    val b3: Boolean = x >= y
    val f: Double = x partialCompare y
    val oi: Option[Int] = x tryCompare y
    val oz0: Option[A] = x pmin y
    val oz1: Option[A] = x pmax y
  }

  def testOrder[A: Order]: Unit = {
    val x = mock[A]
    val y = mock[A]
    val i: Int = x compare y
    val z0: A = x min y
    val z1: A = x max y
  }

  def testInvariantFunctor[F[_]: Invariant, A, B]: Unit = {
    val fa = mock[F[A]]
    val f = mock[A => B]
    val g = mock[B => A]
    val fb: F[B] = fa.imap(f)(g)
  }

  def testInvariantFunctor[F[_]: Contravariant, A, B]: Unit = {
    val fa = mock[F[A]]
    val f = mock[B => A]
    val fb: F[B] = fa.contramap(f)
  }

  def testFoldable[F[_]: Foldable, G[_]: Applicative: MonoidK, A: Monoid, B, Z]: Unit = {
    val fa = mock[F[A]]
    val b = mock[B]
    val f1 = mock[(B, A) => B]
    val b0: B = fa.foldLeft(b)(f1)
    val a0: A = fa.fold

    val f2 = mock[(A, Eval[B]) => Eval[B]]
    val lb0: Eval[B] = fa.foldRight(Now(b))(f2)

    val fz = mock[F[Z]]
    val f3 = mock[Z => A]
    val a1: A = fz.foldMap(f3)

    val f4 = mock[A => G[B]]
    val gu0: G[Unit] = fa.traverse_(f4)

    val fga = mock[F[G[A]]]
    val gu1: G[Unit] = fga.sequence_
    val ga: G[A] = fga.foldK

    val f5 = mock[A => Boolean]
    val oa: Option[A] = fa.find(f5)

    val as0: List[A] = fa.toList
    val as1: List[A] = fa.filter_(f5)
    val as2: List[A] = fa.dropWhile_(f5)
  }

  def testTraverse[F[_]: Traverse, G[_]: Applicative, A]: Unit = {
    val fga = mock[F[G[A]]]
    val gunit: G[F[A]] = fga.sequence
  }

  def testReducible[F[_]: Reducible, G[_]: Apply: SemigroupK, A: Semigroup, B, Z]: Unit = {
    val fa = mock[F[A]]
    val f1 = mock[(A, A) => A]
    val a1: A = fa.reduceLeft(f1)

    val f2 = mock[(A, Eval[A]) => Eval[A]]
    val la: Eval[A] = fa.reduceRight(f2)

    val a2: A = fa.reduce

    val fga = mock[F[G[A]]]
    val ga: G[A] = fga.reduceK

    val fz = mock[F[Z]]
    val f3 = mock[Z => A]
    val a3: A = fz.reduceMap(f3)

    val f4 = mock[A => B]
    val f5 = mock[(B, A) => B]
    val b1: B = fa.reduceLeftTo(f4)(f5)

    val f6 = mock[(A, Eval[B]) => Eval[B]]
    val lb: Eval[B] = fa.reduceRightTo(f4)(f6)

    val f7 = mock[A => G[B]]
    val gu1: G[Unit] = fa.traverse1_(f7)

    val gu2: G[Unit] = fga.sequence1_
  }

  def testFunctor[F[_]: Functor, A, B]: Unit = {
    val fa = mock[F[A]]
    val f = mock[A => B]
    val fb0: F[B] = fa.map(f)
    val fu: F[Unit] = fa.void
    val fab: F[(A, B)] = fa.fproduct(f)

    val b = mock[B]
    val fb1: F[B] = fa.as(b)
  }

  def testApply[F[_]: Apply, A, B, C, D, Z]: Unit = {
    val fa = mock[F[A]]
    val fab = mock[F[A => B]]
    val fb0: F[B] = fab.ap(fa)

    val fb = mock[F[B]]
    val fabz = mock[F[(A, B) => Z]]
    val fz0: F[Z] = fabz.ap2(fa, fb)

    val f = mock[(A, B) => Z]
    val fz1: F[Z] = fa.map2(fb)(f)

    val f1 = mock[(A, B) => Z]
    val ff1 = mock[F[(A, B) => Z]]
    val fz2: F[Z] = (fa |@| fb).map(f1)
    val fz3: F[Z] = (fa |@| fb).apWith(ff1)

    val fc = mock[F[C]]
    val f2 = mock[(A, B, C) => Z]
    val ff2 = mock[F[(A, B, C) => Z]]
    val fz4: F[Z] = (fa |@| fb |@| fc).map(f2)
    val fz5: F[Z] = (fa |@| fb |@| fc).apWith(ff2)
  }

  def testBifoldable[F[_, _]: Bifoldable, A, B, C, D: Monoid]: Unit = {
    val fab = mock[F[A, B]]

    val f0 = mock[(C, A) => C]
    val g0 = mock[(C, B) => C]
    val c0 = fab.bifoldLeft(mock[C])(f0, g0)

    val f1 = mock[(A, Eval[C]) => Eval[C]]
    val g1 = mock[(B, Eval[C]) => Eval[C]]
    val c1 = fab.bifoldRight(mock[Eval[C]])(f1, g1)

    val f2 = mock[A => D]
    val g2 = mock[B => D]
    val d0 = fab.bifoldMap(f2, g2)
  }
}
