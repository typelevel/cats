package cats
package tests

import cats.instances.AllInstances
import cats.syntax.AllSyntax
import cats.functor.{Invariant, Contravariant}

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
object SyntaxTests extends AllInstances with AllSyntax {

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

  def testMonoid[A: Monoid]: Unit = {
    val x = mock[A]
    implicit val y = mock[Eq[A]]
    val z: Boolean = x.isEmpty
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

  def testTraverse[F[_]: Traverse: FlatMap, G[_]: Applicative, A, B]: Unit = {
    val fa = mock[F[A]]
    val f1 = mock[A => G[B]]
    val gfb: G[F[B]] = fa.traverse(f1)

    val f2 = mock[A => G[F[B]]]
    val gfb2: G[F[B]] = fa.traverseM(f2)

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

  def testBitraverse[F[_, _]: Bitraverse, G[_]: Applicative, A, B, C, D]: Unit = {
    val f = mock[A => G[C]]
    val g = mock[B => G[D]]

    val fab = mock[F[A, B]]
    val gfcd = fab.bitraverse(f, g)

    val fgagb = mock[F[G[A], G[B]]]
    val gfab = fgagb.bisequence
  }

  def testMonadCombine[F[_]: MonadCombine, G[_]: Foldable, H[_, _]: Bifoldable, A, B]: Unit = {
    val fga = mock[F[G[A]]]
    val fa = fga.unite

    val fhab = mock[F[H[A, B]]]
    val fafb = fhab.separate
  }

  def testApplicative[F[_]: Applicative, A]: Unit = {
    val a = mock[A]
    val fa = a.pure[F]
  }

  def testApplicativeError[F[_, _], E, A](implicit F: ApplicativeError[F[E, ?], E]): Unit = {
    type G[X] = F[E, X]

    val e = mock[E]
    val ga = e.raiseError[G, A]

    val gea = mock[G[A]]

    val ea = mock[E => A]
    val gea1 = ga.handleError(ea)

    val egea = mock[E => G[A]]
    val gea2 = ga.handleErrorWith(egea)

    val gxea = ga.attempt

    val gxtea = ga.attemptT

    val pfea = mock[PartialFunction[E, A]]
    val gea3 = ga.recover(pfea)

    val pfegea = mock[PartialFunction[E, G[A]]]
    val gea4 = ga.recoverWith(pfegea)
  }

  def testTupleArity[F[_]: Apply : Cartesian, G[_]: Contravariant : Cartesian, H[_]: Invariant : Cartesian, A, B, C, D, E, Z] = {
    val tfabc = mock[(F[A], F[B], F[C])]
    val fa = mock[F[A]]
    val fb = mock[F[B]]
    val fc = mock[F[C]]
    val f = mock[(A, B, C) => Z]
    val ff = mock[F[(A, B, C) => Z]]

    tfabc map3 f
    (fa, fb, fc) map3 f
    (fa, fb, fc) apWith ff

    val tgabc = mock[(G[A], G[B])]
    val ga = mock[G[A]]
    val gb = mock[G[B]]
    val g = mock[Z => (A, B)]

    tgabc contramap2 g
    (ga, gb) contramap2 g

    val thabcde = mock[(H[A], H[B], H[C], H[D], H[E])]
    val ha = mock[H[A]]
    val hb = mock[H[B]]
    val hc = mock[H[C]]
    val hd = mock[H[D]]
    val he = mock[H[E]]
    val f5 = mock[(A, B, C, D, E) => Z]
    val g5 = mock[Z => (A, B, C, D, E)]

    thabcde.imap5(f5)(g5)
    (ha, hb, hc, hd, he).imap5(f5)(g5)
  }
}

/**
 * Similar to [[SyntaxTests]] but doesn't automatically include all
 * instances/syntax, so that piecemeal imports can be tested.
 */
object AdHocSyntaxTests {
  import SyntaxTests.mock

  def testFunctorFilterSyntax[F[_]:FunctorFilter, A]: Unit = {
    import cats.syntax.functorFilter._

    val fa = mock[F[A]]
    val filtered = fa.mapFilter(_ => None)
  }

  def testTraverseFilterSyntax[F[_]:TraverseFilter, G[_]: Applicative, A, B]: Unit = {
    import cats.syntax.traverseFilter._

    val fa = mock[F[A]]
    val f = mock[A => G[Option[B]]]
    val filtered = fa.traverseFilter(f)
  }
}
