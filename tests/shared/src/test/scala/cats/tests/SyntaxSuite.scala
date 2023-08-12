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

package cats.tests

import cats._
import cats.arrow.Compose
import cats.data.{
  Binested,
  EitherNec,
  EitherNel,
  EitherNes,
  EitherT,
  Ior,
  Nested,
  NonEmptyChain,
  NonEmptyList,
  NonEmptySet,
  OptionT,
  Validated,
  ValidatedNec,
  ValidatedNel
}
import cats.syntax.OptionOps
import cats.syntax.all._

import scala.collection.immutable.{SortedMap, SortedSet}

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
object SyntaxSuite {

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
    implicit val y: Eq[A] = mock[Eq[A]]
    val z: Boolean = x.isEmpty
  }

  def testCompose[F[_, _]: Compose, A, B, C, D]: Unit = {
    val x = mock[F[A, B]]
    val y = mock[F[B, C]]
    val z = mock[F[C, D]]

    val a = x >>> y >>> z
    val b = z <<< y <<< x

  }

  def testEq[A: Eq]: Unit = {
    val x = mock[A]
    val y = mock[A]
    val b0: Boolean = x === y
    val b1: Boolean = x =!= y
    val b2: Boolean = x.eqv(y)
    val b3: Boolean = x.neqv(y)
  }

  def testPartialOrder[A: PartialOrder]: Unit = {
    val x = mock[A]
    val y = mock[A]
    val b0: Boolean = x < y
    val b1: Boolean = x <= y
    val b2: Boolean = x > y
    val b3: Boolean = x >= y
    val f: Double = x.partialCompare(y)
    val oi: Option[Int] = x.tryCompare(y)
    val oz0: Option[A] = x.pmin(y)
    val oz1: Option[A] = x.pmax(y)
  }

  def testOrder[A: Order]: Unit = {
    val x = mock[A]
    val y = mock[A]
    val i: Int = x.compare(y)
    val z0: A = x.min(y)
    val z1: A = x.max(y)
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
    val b1: B = fa.foldl(b)(f1)
    val a0: A = fa.fold

    val f2 = mock[(A, Eval[B]) => Eval[B]]
    val lb0: Eval[B] = fa.foldRight(Now(b))(f2)
    val lb1: Eval[B] = fa.foldr(Now(b))(f2)

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

  def testTraverse[F[_]: Traverse: FlatMap, G[_]: Applicative, A, B, C, Z]: Unit = {
    val tfabc = mock[(F[A], F[B], F[C])]
    val fa = mock[F[A]]
    val fb = mock[F[B]]
    val fc = mock[F[C]]
    val f1 = mock[A => G[B]]
    val gfb: G[F[B]] = fa.traverse(f1)

    val f2 = mock[A => G[F[B]]]
    val gfb2: G[F[B]] = fa.flatTraverse(f2)

    val fga = mock[F[G[A]]]
    val gunit: G[F[A]] = fga.sequence

    val ft = mock[(A, B, C) => G[Z]]

    val gfabc = tfabc.traverseN(ft)
    val gfabc2 = (fa, fb, fc).traverseN(ft)
  }

  def testNonEmptyTraverse[F[_]: NonEmptyTraverse: FlatMap, G[_]: Apply: SemigroupK, A: Semigroup, B, Z]: Unit = {
    val fa = mock[F[A]]
    val f1 = mock[A => G[B]]
    val gfb: G[F[B]] = fa.nonEmptyTraverse(f1)

    val f2 = mock[A => G[F[B]]]
    val gfb2: G[F[B]] = fa.nonEmptyFlatTraverse(f2)

    val fga = mock[F[G[A]]]
    val gunit: G[F[A]] = fga.nonEmptySequence
  }

  def testParallel[M[_]: Parallel, T[_]: Traverse, A, B]: Unit = {
    val ta = mock[T[A]]
    val f = mock[A => M[B]]
    val mtb = ta.parTraverse(f)

    val tma = mock[T[M[A]]]
    val mta = tma.parSequence

    val ma = mock[M[A]]

    val mla: M[List[A]] = ma.parReplicateA(mock[Int])
    val mu: M[Unit] = ma.parReplicateA_(mock[Int])
  }

  def testNonEmptyParallel[M[_]: NonEmptyParallel, A, B]: Unit = {
    val ma = mock[M[A]]
    val mb = mock[M[B]]

    val mb2: M[B] = ma &> mb
    val ma2: M[A] = ma <& mb

    val mab = mock[M[A => B]]
    val mb3: M[B] = mab <&> ma

    val ma3: M[A] = ma.parProductL(mb)
    val mb4: M[B] = ma.parProductR(mb)
    val mab2: M[(A, B)] = ma.parProduct(mb)
    val mb5: M[B] = mab.parAp(ma)
  }

  def testParallelUnorderedTraverse[M[_]: Monad, F[_]: CommutativeApplicative, T[_]: UnorderedTraverse: FlatMap, A, B](
    implicit P: Parallel.Aux[M, F]
  ): Unit = {
    val ta = mock[T[A]]
    val f = mock[A => M[B]]
    val mtb = ta.parUnorderedTraverse(f)

    val tma = mock[T[M[A]]]
    val mta = tma.parUnorderedSequence

    val tmta = mock[T[M[T[A]]]]
    val mta2 = tmta.parUnorderedFlatSequence

    val g = mock[A => M[T[B]]]
    val mtb2 = ta.parUnorderedFlatTraverse(g)
  }

  def testParallelFlat[M[_]: Monad, F[_], T[_]: Traverse: FlatMap, A, B](implicit P: Parallel.Aux[M, F]): Unit = {
    val ta = mock[T[A]]
    val f = mock[A => M[T[B]]]
    val mtb = ta.parFlatTraverse(f)

    val tmta = mock[T[M[T[A]]]]
    val mta = tmta.parFlatSequence
  }

  def testParallelTuple[M[_]: Monad, F[_], A, B, C, Z](implicit P: NonEmptyParallel.Aux[M, F]) = {
    val fa = mock[M[A]]
    val fb = mock[M[B]]
    val fc = mock[M[C]]

    val tfabc = mock[(M[A], M[B], M[C])]
    val fthree = mock[(A, B, C) => Z]
    val mfthree = mock[(A, B, C) => M[Z]]

    tfabc.parMapN(fthree)
    (fa, fb, fc).parMapN(fthree)
    tfabc.parFlatMapN(mfthree)
    (fa, fb, fc).parFlatMapN(mfthree)

    val tfab = mock[(M[A], M[B])]
    val ftwo = mock[(A, B) => Z]
    val mftwo = mock[(A, B) => M[Z]]

    tfab.parMapN(ftwo)
    (fa, fb).parMapN(ftwo)
    tfab.parFlatMapN(mftwo)
    (fa, fb).parFlatMapN(mftwo)

    val tfa = mock[Tuple1[M[A]]]
    val mfone = mock[A => M[Z]]

    tfa.parFlatMap(mfone)
  }

  def testParallelBi[M[_], F[_], T[_, _]: Bitraverse, A, B, C, D](implicit P: Parallel.Aux[M, F]): Unit = {
    val tab = mock[T[A, B]]
    val f = mock[A => M[C]]
    val g = mock[B => M[D]]
    val mtcd = tab.parBitraverse(f, g)
    val mtcb = tab.parLeftTraverse(f)

    val tmamb = mock[T[M[A], M[B]]]
    val mtab1 = tmamb.parBisequence

    val tmab = mock[T[M[A], B]]
    val mtab2 = tmab.parLeftSequence
  }

  def testParallelFoldable[T[_]: Foldable, M[_]: Parallel, A, B: Monoid]: Unit = {
    val ta = mock[T[A]]
    val f = mock[A => M[B]]
    val mb = ta.parFoldMapA(f)
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
    val gu1: G[Unit] = fa.nonEmptyTraverse_(f7)

    val gu2: G[Unit] = fga.nonEmptySequence_
  }

  def testFunctor[F[_]: Functor, A, B]: Unit = {
    val fa = mock[F[A]]
    val f = mock[A => B]
    val fb0: F[B] = fa.map(f)
    val fu: F[Unit] = fa.void
    val fab: F[(A, B)] = fa.fproduct(f)
    val fba: F[(B, A)] = fa.fproductLeft(f)

    val b = mock[B]
    val fb1: F[B] = fa.as(b)

    val c = mock[F[Boolean]]
    c.ifF(1, 0)
  }

  def testApply[F[_]: Apply: Semigroupal,
                G[_]: Contravariant: Semigroupal,
                H[_]: Invariant: Semigroupal,
                A,
                B,
                C,
                D,
                E,
                Z
  ] = {
    val tfabc = mock[(F[A], F[B], F[C])]
    val fa = mock[F[A]]
    val fb = mock[F[B]]
    val fc = mock[F[C]]
    val f = mock[(A, B, C) => Z]
    val ff = mock[F[(A, B, C) => Z]]

    fa.productR(fb)
    fa.productL(fb)
    fa *> fb
    fb <* fc

    tfabc.mapN(f)
    (fa, fb, fc).mapN(f)
    (fa, fb, fc).apWith(ff)

    val tgabc = mock[(G[A], G[B])]
    val ga = mock[G[A]]
    val gb = mock[G[B]]
    val g = mock[Z => (A, B)]

    tgabc.contramapN(g)
    (ga, gb).contramapN(g)

    val thabcde = mock[(H[A], H[B], H[C], H[D], H[E])]
    val ha = mock[H[A]]
    val hb = mock[H[B]]
    val hc = mock[H[C]]
    val hd = mock[H[D]]
    val he = mock[H[E]]
    val f5 = mock[(A, B, C, D, E) => Z]
    val g5 = mock[Z => (A, B, C, D, E)]

    thabcde.imapN(f5)(g5)
    (ha, hb, hc, hd, he).imapN(f5)(g5)

    val tfab = mock[F[A => B]]
    tfab.ap(fa)
    tfab <*> fa

    val tabcf = mock[F[(A, B) => C]]
    tabcf.ap2(fa, fb)

    val tabc = mock[(A, B) => C]
    fa.map2(fb)(tabc)

    val tEvalfb = mock[Eval[F[B]]]
    fa.map2Eval(tEvalfb)(tabc)
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
    val gfcb = fab.leftTraverse(f)

    val fgagb = mock[F[G[A], G[B]]]
    val gfab = fgagb.bisequence

    val fgab = mock[F[G[A], B]]
    val gfab2 = fgab.leftSequence
  }

  def testNonEmptyAlternative[F[_]: NonEmptyAlternative, A]: Unit = {
    val fa = mock[F[A]]
    val a = mock[A]

    val fa1: F[A] = fa.prependK(a)
    val fa2: F[A] = fa.appendK(a)
  }

  def testAlternativeFlatMap[F[_]: Alternative: FlatMap, G[_]: Foldable, H[_, _]: Bifoldable, A, B]: Unit = {
    val fga = mock[F[G[A]]]
    val fa = fga.unite

    val fhab = mock[F[H[A, B]]]
    val fafb = fhab.separate
  }

  def testAlternativeFoldable[F[_]: Alternative: Foldable, G[_]: Foldable, H[_, _]: Bifoldable, A, B]: Unit = {
    val fhab = mock[F[H[A, B]]]
    val fafb = fhab.separateFoldable
  }

  def testApplicative[F[_]: Applicative, A]: Unit = {
    val a = mock[A]
    val fa = a.pure[F]
    val replicateA = fa.replicateA(1)
    val replicateA_ = fa.replicateA_(1)
  }

  def testFlatMap[F[_]: FlatMap, A, B, C, D, Z]: Unit = {
    val a = mock[A]
    val returnValue = mock[F[Either[A, B]]]
    val done = a.tailRecM[F, B](a => returnValue)
    val tfabc = mock[(F[A], F[B], F[C])]
    val ff = mock[(A, B, C) => F[Z]]

    tfabc.flatMapN(ff)

    val tfa = mock[Tuple1[F[A]]]
    val ffone = mock[A => F[Z]]

    tfa.flatMap(ffone)

    val x = mock[Function[A, F[B]]]
    val y = mock[Function[B, F[C]]]
    val z = mock[Function[C, F[D]]]

    val b = x.andThenF(y).andThenF(z)
    val b2 = x >=> y >=> z

    val c = z.composeF(y).composeF(x)
    val c2 = z <=< y <=< x
  }

  def testApplicativeError[F[_, _], E, A, B](implicit F: ApplicativeError[F[E, *], E]): Unit = {
    type G[X] = F[E, X]

    val e = mock[E]
    val ga = e.raiseError[G, A]

    val gea = mock[G[A]]

    val ea = mock[E => A]
    val gea1 = ga.handleError(ea)

    val geu = mock[G[Unit]]
    val veu = geu.voidError

    val egea = mock[E => G[A]]
    val gea2 = ga.handleErrorWith(egea)

    val gxea = ga.attempt

    val gxtea = ga.attemptT

    val pfea = mock[PartialFunction[E, A]]
    val gea3 = ga.recover(pfea)

    val pfegea = mock[PartialFunction[E, G[A]]]
    val gea4 = ga.recoverWith(pfegea)

    val eb = mock[E => B]
    val ab = mock[A => B]
    val gb: G[B] = gea.redeem(eb, ab)

    val pfee = mock[PartialFunction[E, E]]
    val gea5 = gea.adaptErr(pfee)
  }

  def testApplicativeErrorSubtype[F[_], A](implicit F: ApplicativeError[F, CharSequence]): Unit = {
    val fea = "meow".raiseError[F, A]
  }

  def testMonadError[F[_, _], E, A, B](implicit F: MonadError[F[E, *], E]): Unit = {
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

    val eb = mock[E => B]
    val ab = mock[A => B]
    val gb: G[B] = gea.redeem(eb, ab)

    val efb = mock[E => G[B]]
    val afb = mock[A => G[B]]
    val gb2: G[B] = gea.redeemWith(efb, afb)

    val pfee = mock[PartialFunction[E, E]]
    val gea5 = gea.adaptError(pfee)
  }

  def testNested[F[_], G[_], A]: Unit = {
    val fga: F[G[A]] = mock[F[G[A]]]

    val nested: Nested[F, G, A] = fga.nested
  }

  def testEither[F[_]: Applicative, A: Order, B: Order](): Unit = {
    val either = mock[Either[A, B]]
    val a = mock[A]
    val b = mock[B]

    val v1: Validated[A, B] = either.toValidated
    val v2: ValidatedNel[A, B] = either.toValidatedNel
    val v3: Validated[F[A], B] = either.toValidated.leftLiftTo[F]

    val v4: Either[F[A], B] = either.leftLiftTo[F]
    val v5: EitherT[F, A, B] = either.toEitherT
    val v6: EitherNel[A, B] = either.toEitherNel
    val v7: EitherNec[A, B] = either.toEitherNec
    val v8: EitherNes[A, B] = either.toEitherNes

    val v9: Either[A, B] = Either.left[A, B](a)
    val v10: Either[A, B] = Either.right[A, B](b)

    val v11: EitherNec[A, Nothing] = Either.leftNec(a)
    val v12: EitherNec[Nothing, B] = Either.rightNec(b)
    val v13: EitherNes[A, Nothing] = Either.leftNes(a)
    val v14: EitherNes[Nothing, B] = Either.rightNes(b)
    val v15: EitherNel[A, Nothing] = Either.leftNel(a)
    val v16: EitherNel[Nothing, B] = Either.rightNel(b)
  }

  def testOption[F[_]: Applicative, G[_], A, B: Monoid](): Unit = {
    val option = mock[Option[B]]
    val a = mock[A]
    val b = mock[B]
    implicit val aega: ApplicativeError[G, B] = mock[ApplicativeError[G, B]]

    val v1: Validated[B, A] = option.toInvalid(a)
    val v2: ValidatedNel[B, A] = option.toInvalidNel(a)
    val v3: Validated[F[B], A] = option.toInvalid(a).leftLiftTo[F]
    val v4: ValidatedNec[B, A] = option.toInvalidNec(a)
    val v5: Validated[B, B] = option.toValid(b)
    val v6: ValidatedNel[B, B] = option.toValidNel(b)
    val v7: ValidatedNec[B, B] = option.toValidNec(b)
    val v8: Ior[B, B] = option.toRightIor(b)
    val v9: Ior[B, A] = option.toLeftIor(a)
    val v10: EitherNel[B, B] = option.toRightNel(b)
    val v11: EitherNec[B, B] = option.toRightNec(b)
    val v12: EitherNel[B, A] = option.toLeftNel(a)
    val v13: EitherNec[B, A] = option.toLeftNec(a)
    val v14: B = option.orEmpty
    val v15 = option.liftTo[F]
    val v16: G[Unit] = option.raiseTo[G]
    val v17: OptionT[F, B] = option.toOptionT[F]
  }

  def testValidated[F[_]: Applicative, A: Order, B: Order: Monoid](): Unit = {
    val validated = mock[Validated[A, B]]
    val a = mock[A]
    val b = mock[B]

    val v1: Validated[A, B] = b.valid[A]
    val v2: Validated[F[A], A] = a.invalid[A].leftLiftTo[F]
    val v3: ValidatedNel[A, B] = b.validNel[A]
    val v4: Validated[A, B] = a.invalid[B]
    val v5: ValidatedNel[A, B] = a.invalidNel[B]
  }

  def testBinested[F[_, _], G[_], H[_], A, B]: Unit = {
    val fgahb = mock[F[G[A], H[B]]]

    val binested: Binested[F, G, H, A, B] = fgahb.binested
  }

  def testNonEmptySet[A, B: Order]: Unit = {
    val f = mock[A => B]
    val set = mock[SortedSet[A]]

    val nes: Option[NonEmptySet[A]] = set.toNes
    val grouped: SortedMap[B, NonEmptySet[A]] = set.groupByNes(f)
  }

  def testAlign[F[_]: Align, A, B, C]: Unit = {
    import cats.data.Ior
    val fa = mock[F[A]]
    val fb = mock[F[B]]
    val f = mock[A Ior B => C]
    val f2 = mock[(Option[A], Option[B]) => C]
    val f3 = mock[(A, A) => A]
    val a = mock[A]
    val b = mock[B]

    val fab = fa.align(fb)
    val fc = fa.alignWith(fb)(f)

    val padZipped = fa.padZip(fb)
    val padZippedWith = fa.padZipWith(fb)(f2)

    implicit val sa: Semigroup[A] = mock[Semigroup[A]]
    val fa2 = fa.alignCombine(fa)
    val fa3 = fa.alignMergeWith(fa)(f3)

    val zippedAll = fa.zipAll(fb, a, b)
  }

  def testNonEmptyList[A, B: Order]: Unit = {
    val f = mock[A => B]
    val list = mock[List[A]]

    val nel: Option[NonEmptyList[A]] = list.toNel
    val grouped: SortedMap[B, NonEmptyList[A]] = list.groupByNel(f)
  }

  def testNonEmptyChain[A, B: Order]: Unit = {
    val f = mock[A => B]
    val list = mock[List[A]]

    val grouped: SortedMap[B, NonEmptyChain[A]] = list.groupByNec(f)
  }

  def testSequenceFilter[A, B]: Unit = {
    val f = mock[List[Either[A, Option[B]]]]

    val result: Either[A, List[B]] = f.sequenceFilter
  }

  def testTraverseCollect[A, B]: Unit = {
    val list = mock[List[A]]
    val f = mock[PartialFunction[A, Option[B]]]

    val result: Option[List[B]] = list.traverseCollect(f)
  }

  def testSemigroupal[F[_]: Semigroupal, A, B]: Unit = {
    val fa = mock[F[A]]
    val fb = mock[F[B]]

    fa.product(fb)
    fa |@| fb
  }
}
