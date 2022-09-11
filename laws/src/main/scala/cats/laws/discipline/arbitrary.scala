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

package cats
package laws
package discipline
import kernel.compat.scalaVersionSpecific._
import cats.data.NonEmptyList.ZipNonEmptyList
import cats.data.NonEmptyVector.ZipNonEmptyVector

import scala.util.{Failure, Success, Try}
import scala.collection.immutable.{Seq, SortedMap, SortedSet}
import cats.data._
import org.scalacheck.{Arbitrary, Cogen, Gen}
import org.scalacheck.Arbitrary.{arbitrary => getArbitrary}

/**
 * Arbitrary instances for cats.data
 */
@suppressUnusedImportWarningForScalaVersionSpecific
object arbitrary extends ArbitraryInstances0 with ScalaVersionSpecific.ArbitraryInstances {

  // this instance is not available in ScalaCheck 1.13.2.
  // remove this once a newer version is available.
  implicit val catsLawsCogenForThrowable: Cogen[Throwable] =
    Cogen[String].contramap(_.toString)

  // this instance is not available in ScalaCheck 1.13.2.
  // remove this once a newer version is available.
  implicit def catsLawsCogenForTry[A](implicit A: Cogen[A]): Cogen[Try[A]] =
    Cogen((seed, x) =>
      x match {
        case Success(a) => A.perturb(seed, a)
        case Failure(e) => Cogen[Throwable].perturb(seed, e)
      }
    )

  // this instance is not available in ScalaCheck 1.13.2.
  // remove this once a newer version is available.
  implicit def catsLawsCogenForFunction0[A](implicit A: Cogen[A]): Cogen[Function0[A]] =
    A.contramap(_())

  implicit def catsLawsArbitraryForConst[A, B](implicit A: Arbitrary[A]): Arbitrary[Const[A, B]] =
    Arbitrary(A.arbitrary.map(Const[A, B]))

  implicit def catsLawsCogenForConst[A, B](implicit A: Cogen[A]): Cogen[Const[A, B]] =
    A.contramap(_.getConst)

  implicit def catsLawsArbitraryForOneAnd[F[_], A](implicit
    A: Arbitrary[A],
    F: Arbitrary[F[A]]
  ): Arbitrary[OneAnd[F, A]] =
    Arbitrary(F.arbitrary.flatMap(fa => A.arbitrary.map(a => OneAnd(a, fa))))

  implicit def catsLawsCogenForOneAnd[F[_], A](implicit A: Cogen[A], F: Cogen[F[A]]): Cogen[OneAnd[F, A]] =
    Cogen((seed, x) => F.perturb(A.perturb(seed, x.head), x.tail))

  implicit def catsLawsArbitraryForNonEmptySeq[A](implicit A: Arbitrary[A]): Arbitrary[NonEmptySeq[A]] =
    Arbitrary(implicitly[Arbitrary[Seq[A]]].arbitrary.flatMap(fa => A.arbitrary.map(a => NonEmptySeq(a, fa))))

  implicit def catsLawsCogenForNonEmptySeq[A](implicit A: Cogen[A]): Cogen[NonEmptySeq[A]] =
    Cogen[Seq[A]].contramap(_.toSeq)

  implicit def catsLawsArbitraryForNonEmptyVector[A](implicit A: Arbitrary[A]): Arbitrary[NonEmptyVector[A]] =
    Arbitrary(implicitly[Arbitrary[Vector[A]]].arbitrary.flatMap(fa => A.arbitrary.map(a => NonEmptyVector(a, fa))))

  implicit def catsLawsCogenForNonEmptyVector[A](implicit A: Cogen[A]): Cogen[NonEmptyVector[A]] =
    Cogen[Vector[A]].contramap(_.toVector)

  implicit def catsLawsArbitraryForNonEmptySet[A: Order](implicit A: Arbitrary[A]): Arbitrary[NonEmptySet[A]] =
    Arbitrary(implicitly[Arbitrary[SortedSet[A]]].arbitrary.flatMap(fa => A.arbitrary.map(a => NonEmptySet(a, fa))))

  implicit def catsLawsCogenForNonEmptySet[A: Order: Cogen]: Cogen[NonEmptySet[A]] =
    Cogen[SortedSet[A]].contramap(_.toSortedSet)

  implicit def catsLawsArbitraryForZipSeq[A](implicit A: Arbitrary[A]): Arbitrary[ZipSeq[A]] =
    Arbitrary(implicitly[Arbitrary[Seq[A]]].arbitrary.map(v => new ZipSeq(v)))

  implicit def catsLawsArbitraryForZipVector[A](implicit A: Arbitrary[A]): Arbitrary[ZipVector[A]] =
    Arbitrary(implicitly[Arbitrary[Vector[A]]].arbitrary.map(v => new ZipVector(v)))

  implicit def catsLawsArbitraryForZipList[A](implicit A: Arbitrary[A]): Arbitrary[ZipList[A]] =
    Arbitrary(implicitly[Arbitrary[List[A]]].arbitrary.map(v => new ZipList(v)))

  implicit def catsLawsArbitraryForZipNonEmptyVector[A](implicit A: Arbitrary[A]): Arbitrary[ZipNonEmptyVector[A]] =
    Arbitrary(implicitly[Arbitrary[NonEmptyVector[A]]].arbitrary.map(nev => new ZipNonEmptyVector(nev)))

  implicit def catsLawsArbitraryForNonEmptyList[A](implicit A: Arbitrary[A]): Arbitrary[NonEmptyList[A]] =
    Arbitrary(implicitly[Arbitrary[List[A]]].arbitrary.flatMap(fa => A.arbitrary.map(a => NonEmptyList(a, fa))))

  implicit def catsLawsCogenForNonEmptyList[A](implicit A: Cogen[A]): Cogen[NonEmptyList[A]] =
    Cogen[List[A]].contramap(_.toList)

  implicit def catsLawsArbitraryForNonEmptyChain[A](implicit A: Arbitrary[A]): Arbitrary[NonEmptyChain[A]] =
    Arbitrary(implicitly[Arbitrary[Chain[A]]].arbitrary.flatMap { chain =>
      NonEmptyChain.fromChain(chain) match {
        case None     => A.arbitrary.map(NonEmptyChain.one)
        case Some(ne) => Gen.const(ne)
      }
    })

  implicit def catsLawsCogenForNonEmptyChain[A](implicit A: Cogen[A]): Cogen[NonEmptyChain[A]] =
    Cogen[Chain[A]].contramap(_.toChain)

  implicit def catsLawsArbitraryForZipNonEmptyList[A](implicit A: Arbitrary[A]): Arbitrary[ZipNonEmptyList[A]] =
    Arbitrary(implicitly[Arbitrary[NonEmptyList[A]]].arbitrary.map(nel => new ZipNonEmptyList(nel)))

  implicit def arbNonEmptyMap[K: Order, A](implicit A: Arbitrary[A], K: Arbitrary[K]): Arbitrary[NonEmptyMap[K, A]] =
    Arbitrary(for {
      fa <- implicitly[Arbitrary[SortedMap[K, A]]].arbitrary
      k <- K.arbitrary
      a <- A.arbitrary
    } yield NonEmptyMap((k, a), fa))

  @deprecated("Preserved for bincompat", "2.9.0")
  def cogenNonEmptyMap[K, A](kOrder: Order[K],
                             kCogen: Cogen[K],
                             aOrder: Order[A],
                             aCogen: Cogen[A]
  ): Cogen[NonEmptyMap[K, A]] = {
    implicit val orderingK: Order[K] = kOrder
    implicit val cogenK: Cogen[K] = kCogen
    implicit val cogenA: Cogen[A] = aCogen

    cogenNonEmptyMap[K, A]
  }

  implicit def cogenNonEmptyMap[K: Order: Cogen, A: Cogen]: Cogen[NonEmptyMap[K, A]] =
    Cogen[SortedMap[K, A]].contramap(_.toSortedMap)

  implicit def catsLawsArbitraryForEitherT[F[_], A, B](implicit
    F: Arbitrary[F[Either[A, B]]]
  ): Arbitrary[EitherT[F, A, B]] =
    Arbitrary(F.arbitrary.map(EitherT(_)))

  implicit def catsLawsCogenForEitherT[F[_], A, B](implicit F: Cogen[F[Either[A, B]]]): Cogen[EitherT[F, A, B]] =
    F.contramap(_.value)

  implicit def catsLawsArbitraryForValidated[A, B](implicit
    A: Arbitrary[A],
    B: Arbitrary[B]
  ): Arbitrary[Validated[A, B]] =
    Arbitrary(Gen.oneOf(A.arbitrary.map(Validated.invalid), B.arbitrary.map(Validated.valid)))

  implicit def catsLawsCogenForValidated[A, B](implicit A: Cogen[A], B: Cogen[B]): Cogen[Validated[A, B]] =
    Cogen((seed, x) => x.fold(A.perturb(seed, _), B.perturb(seed, _)))

  implicit def catsLawsArbitraryForIor[A, B](implicit A: Arbitrary[A], B: Arbitrary[B]): Arbitrary[A Ior B] =
    Arbitrary(
      Gen.oneOf(A.arbitrary.map(Ior.left),
                B.arbitrary.map(Ior.right),
                for {
                  a <- A.arbitrary; b <- B.arbitrary
                } yield Ior.both(a, b)
      )
    )

  implicit def catsLawsCogenForIor[A, B](implicit A: Cogen[A], B: Cogen[B]): Cogen[A Ior B] =
    Cogen((seed, x) => x.fold(A.perturb(seed, _), B.perturb(seed, _), (a, b) => A.perturb(B.perturb(seed, b), a)))

  implicit def catsLawsArbitraryForIorT[F[_], A, B](implicit F: Arbitrary[F[Ior[A, B]]]): Arbitrary[IorT[F, A, B]] =
    Arbitrary(F.arbitrary.map(IorT(_)))

  implicit def catsLawsCogenForIorT[F[_], A, B](implicit F: Cogen[F[Ior[A, B]]]): Cogen[IorT[F, A, B]] =
    F.contramap(_.value)

  implicit def catsLawsArbitraryForOptionT[F[_], A](implicit F: Arbitrary[F[Option[A]]]): Arbitrary[OptionT[F, A]] =
    Arbitrary(F.arbitrary.map(OptionT.apply))

  implicit def catsLawsCogenForOptionT[F[_], A](implicit F: Cogen[F[Option[A]]]): Cogen[OptionT[F, A]] =
    F.contramap(_.value)

  implicit def catsLawsArbitraryForRepresentableStoreT[W[_], F[_], S, A](implicit
    W: Arbitrary[W[F[A]]],
    S: Arbitrary[S],
    F: Representable.Aux[F, S]
  ): Arbitrary[RepresentableStoreT[W, F, S, A]] =
    Arbitrary(
      for {
        runF <- W.arbitrary
        index <- S.arbitrary
      } yield RepresentableStoreT(runF, index)
    )

  implicit def catsLawsCogenForRepresentableStoreT[W[_], F[_], S, A](implicit
    W: Cogen[W[F[A]]],
    S: Cogen[S]
  ): Cogen[RepresentableStoreT[W, F, S, A]] =
    Cogen((seed, st) => S.perturb(W.perturb(seed, st.runF), st.index))

  implicit def catsLawsArbitraryForIdT[F[_], A](implicit F: Arbitrary[F[A]]): Arbitrary[IdT[F, A]] =
    Arbitrary(F.arbitrary.map(IdT.apply))

  implicit def catsLawsCogenForIdT[F[_], A](implicit F: Cogen[F[A]]): Cogen[IdT[F, A]] =
    F.contramap(_.value)

  implicit def catsLawsArbitraryForEval[A: Arbitrary]: Arbitrary[Eval[A]] =
    Arbitrary(
      Gen.oneOf(getArbitrary[A].map(a => Eval.now(a)),
                getArbitrary[() => A].map(f => Eval.later(f())),
                getArbitrary[() => A].map(f => Eval.always(f()))
      )
    )

  implicit def catsLawsCogenForEval[A: Cogen]: Cogen[Eval[A]] =
    Cogen[A].contramap(_.value)

  implicit def catsLawsArbitraryForTuple2K[F[_], G[_], A](implicit
    F: Arbitrary[F[A]],
    G: Arbitrary[G[A]]
  ): Arbitrary[Tuple2K[F, G, A]] =
    Arbitrary(F.arbitrary.flatMap(fa => G.arbitrary.map(ga => Tuple2K[F, G, A](fa, ga))))

  implicit def catsLawsArbitraryForFunc[F[_], A, B](implicit
    AA: Arbitrary[A],
    CA: Cogen[A],
    F: Arbitrary[F[B]]
  ): Arbitrary[Func[F, A, B]] =
    Arbitrary(Arbitrary.arbitrary[A => F[B]].map(Func.func))

  implicit def catsLawsArbitraryForAppFunc[F[_], A, B](implicit
    AA: Arbitrary[A],
    CA: Cogen[A],
    F: Arbitrary[F[B]],
    FF: Applicative[F]
  ): Arbitrary[AppFunc[F, A, B]] =
    Arbitrary(Arbitrary.arbitrary[A => F[B]].map(Func.appFunc(_)))

  implicit def catsLawsArbitraryForWriter[L: Arbitrary, V: Arbitrary]: Arbitrary[Writer[L, V]] =
    catsLawsArbitraryForWriterT[Id, L, V]

  implicit def catsLawsCogenForWriter[L: Cogen, V: Cogen]: Cogen[Writer[L, V]] =
    Cogen[(L, V)].contramap(_.run)

  // until this is provided by ScalaCheck
  implicit def catsLawsArbitraryForPartialFunction[A, B](implicit
    F: Arbitrary[A => Option[B]]
  ): Arbitrary[PartialFunction[A, B]] =
    Arbitrary(F.arbitrary.map(Function.unlift))

  implicit def catsLawsArbitraryForEitherK[F[_], G[_], A](implicit
    F: Arbitrary[F[A]],
    G: Arbitrary[G[A]]
  ): Arbitrary[EitherK[F, G, A]] =
    Arbitrary(Gen.oneOf(F.arbitrary.map(EitherK.leftc[F, G, A]), G.arbitrary.map(EitherK.rightc[F, G, A])))

  implicit def catsLawsCogenForEitherK[F[_], G[_], A](implicit
    F: Cogen[F[A]],
    G: Cogen[G[A]]
  ): Cogen[EitherK[F, G, A]] =
    Cogen((seed, x) => x.run.fold(F.perturb(seed, _), G.perturb(seed, _)))

  implicit def catLawsCogenForTuple2K[F[_], G[_], A](implicit F: Cogen[F[A]], G: Cogen[G[A]]): Cogen[Tuple2K[F, G, A]] =
    Cogen((seed, t) => F.perturb(G.perturb(seed, t.second), t.first))

  implicit def catsLawsArbitraryForShow[A: Arbitrary]: Arbitrary[Show[A]] =
    Arbitrary(Show.fromToString[A])

  implicit def catsLawsArbitraryForFn0[A: Arbitrary]: Arbitrary[() => A] =
    Arbitrary(getArbitrary[A].map(() => _))

  // TODO: we should probably be using Cogen for generating Eq, Order,
  // etc. however, we'd still have to ensure that (x.## == y.##)
  // implies equal, in order to avoid producing invalid instances.

  implicit def catsLawsArbitraryForEq[A: Arbitrary]: Arbitrary[Eq[A]] =
    Arbitrary(getArbitrary[Int => Int].map(f => Eq.by(x => f(x.##))))

  implicit def catsLawsArbitraryForEquiv[A: Arbitrary]: Arbitrary[Equiv[A]] =
    Arbitrary(getArbitrary[Eq[A]].map(Eq.catsKernelEquivForEq(_)))

  implicit def catsLawsArbitraryForPartialOrder[A: Arbitrary]: Arbitrary[PartialOrder[A]] =
    Arbitrary(getArbitrary[Int => Double].map(f => PartialOrder.by(x => f(x.##))))

  implicit def catsLawsArbitraryForPartialOrdering[A: Arbitrary]: Arbitrary[PartialOrdering[A]] =
    Arbitrary(getArbitrary[PartialOrder[A]].map(PartialOrder.catsKernelPartialOrderingForPartialOrder(_)))

  implicit def catsLawsArbitraryForOrder[A: Arbitrary]: Arbitrary[Order[A]] =
    Arbitrary(getArbitrary[Int => Int].map(f => Order.by(x => f(x.##))))

  implicit def catsLawsArbitraryForSortedMap[K: Arbitrary: Order, V: Arbitrary]: Arbitrary[SortedMap[K, V]] =
    Arbitrary(getArbitrary[Map[K, V]].map(s => SortedMap.empty[K, V](implicitly[Order[K]].toOrdering) ++ s))

  @deprecated("Preserved for bincompat", "2.9.0")
  def catsLawsCogenForSortedMap[K, V](kOrder: Order[K],
                                      kCogen: Cogen[K],
                                      vOrder: Order[V],
                                      vCogen: Cogen[V]
  ): Cogen[SortedMap[K, V]] = {
    implicit val orderingK: Order[K] = kOrder
    implicit val cogenK: Cogen[K] = kCogen
    implicit val cogenA: Cogen[V] = vCogen

    catsLawsCogenForSortedMap[K, V]
  }

  implicit def catsLawsCogenForSortedMap[K: Order: Cogen, V: Cogen]: Cogen[SortedMap[K, V]] = {
    implicit val orderingK: Ordering[K] = Order[K].toOrdering

    implicitly[Cogen[Map[K, V]]].contramap(_.toMap)
  }

  implicit def catsLawsArbitraryForSortedSet[A: Arbitrary: Order]: Arbitrary[SortedSet[A]] =
    Arbitrary(getArbitrary[Set[A]].map(s => SortedSet.empty[A](implicitly[Order[A]].toOrdering) ++ s))

  implicit def catsLawsCogenForSortedSet[A: Order: Cogen]: Cogen[SortedSet[A]] = {
    implicit val orderingA: Ordering[A] = Order[A].toOrdering

    implicitly[Cogen[Set[A]]].contramap(_.toSet)
  }

  implicit def catsLawsArbitraryForOrdering[A: Arbitrary]: Arbitrary[Ordering[A]] =
    Arbitrary(getArbitrary[Order[A]].map(Order.catsKernelOrderingForOrder(_)))

  implicit def catsLawsArbitraryForHash[A: Hash]: Arbitrary[Hash[A]] =
    Arbitrary(Hash.fromUniversalHashCode[A])

  implicit def catsLawsArbitraryForNested[F[_], G[_], A](implicit FG: Arbitrary[F[G[A]]]): Arbitrary[Nested[F, G, A]] =
    Arbitrary(FG.arbitrary.map(Nested(_)))

  implicit def catsLawsArbitraryForBinested[F[_, _], G[_], H[_], A, B](implicit
    F: Arbitrary[F[G[A], H[B]]]
  ): Arbitrary[Binested[F, G, H, A, B]] =
    Arbitrary(F.arbitrary.map(Binested(_)))

  implicit def catsLawArbitraryForState[S: Arbitrary: Cogen, A: Arbitrary]: Arbitrary[State[S, A]] =
    catsLawArbitraryForIndexedStateT[Eval, S, S, A]

  implicit def catsLawArbitraryForReader[A: Arbitrary: Cogen, B: Arbitrary]: Arbitrary[Reader[A, B]] =
    catsLawsArbitraryForKleisli[Id, A, B]

  implicit def catsLawArbitraryForCokleisliId[A: Arbitrary: Cogen, B: Arbitrary]: Arbitrary[Cokleisli[Id, A, B]] =
    catsLawsArbitraryForCokleisli[Id, A, B]

  implicit def catsLawsArbitraryForOp[Arr[_, _], A, B](implicit Arr: Arbitrary[Arr[B, A]]): Arbitrary[Op[Arr, A, B]] =
    Arbitrary(Arr.arbitrary.map(Op(_)))

  implicit def catsLawsCogenForOp[Arr[_, _], A, B](implicit Arr: Cogen[Arr[B, A]]): Cogen[Op[Arr, A, B]] =
    Arr.contramap(_.run)

  implicit def catsLawsArbitraryForIRWST[F[_]: Applicative, E, L, SA, SB, A](implicit
    F: Arbitrary[(E, SA) => F[(L, SB, A)]]
  ): Arbitrary[IndexedReaderWriterStateT[F, E, L, SA, SB, A]] =
    Arbitrary(F.arbitrary.map(IndexedReaderWriterStateT(_)))

  implicit def catsLawsArbitraryForRepresentableStore[F[_], S, A](implicit
    R: Representable.Aux[F, S],
    ArbS: Arbitrary[S],
    ArbFA: Arbitrary[F[A]]
  ): Arbitrary[RepresentableStore[F, S, A]] =
    Arbitrary {
      for {
        fa <- ArbFA.arbitrary
        s <- ArbS.arbitrary
      } yield {
        RepresentableStore[F, S, A](fa, s)
      }
    }

  implicit def catsLawsCogenForRepresentableStore[F[_]: Representable, S, A](implicit
    CA: Cogen[A]
  ): Cogen[RepresentableStore[F, S, A]] =
    CA.contramap(_.extract)

  implicit def catsLawsArbitraryForAndThen[A, B](implicit F: Arbitrary[A => B]): Arbitrary[AndThen[A, B]] =
    Arbitrary(F.arbitrary.map(AndThen(_)))

  implicit def catsLawsCogenForAndThen[A, B](implicit F: Cogen[A => B]): Cogen[AndThen[A, B]] =
    Cogen((seed, x) => F.perturb(seed, x))

  implicit def catsLawsArbitraryForChain[A](implicit A: Arbitrary[A]): Arbitrary[Chain[A]] = {
    val genA = A.arbitrary

    def genSize(sz: Int): Gen[Chain[A]] = {
      val fromSeq = Gen.listOfN(sz, genA).map(Chain.fromSeq)
      val recursive =
        sz match {
          case 0 => Gen.const(Chain.nil)
          case 1 => genA.map(Chain.one)
          case n =>
            // Here we concat two chains
            for {
              n0 <- Gen.choose(1, n - 1)
              n1 = n - n0
              left <- genSize(n0)
              right <- genSize(n1)
            } yield left ++ right
        }

      // prefer to generate recursively built Chains
      // but sometimes create fromSeq
      Gen.frequency((5, recursive), (1, fromSeq))
    }

    Arbitrary(Gen.sized(genSize))
  }

  implicit def catsLawsCogenForChain[A](implicit A: Cogen[A]): Cogen[Chain[A]] =
    Cogen[List[A]].contramap(_.toList)

  implicit val catsLawsCogenForMiniInt: Cogen[MiniInt] =
    Cogen[Int].contramap(_.toInt)

  implicit val catsLawsArbitraryForMiniInt: Arbitrary[MiniInt] =
    Arbitrary(Gen.oneOf(MiniInt.allValues))
}

sealed private[discipline] trait ArbitraryInstances0 {

  implicit def catsLawArbitraryForIndexedStateT[F[_], SA, SB, A](implicit
    F: Arbitrary[F[SA => F[(SB, A)]]]
  ): Arbitrary[IndexedStateT[F, SA, SB, A]] =
    Arbitrary(F.arbitrary.map(IndexedStateT.applyF))

  implicit def catsLawsArbitraryForWriterT[F[_], L, V](implicit F: Arbitrary[F[(L, V)]]): Arbitrary[WriterT[F, L, V]] =
    Arbitrary(F.arbitrary.map(WriterT(_)))

  implicit def catsLawsCogenForWriterT[F[_], L, V](implicit F: Cogen[F[(L, V)]]): Cogen[WriterT[F, L, V]] =
    F.contramap(_.run)

  implicit def catsLawsArbitraryForKleisli[F[_], A, B](implicit
    AA: Arbitrary[A],
    CA: Cogen[A],
    F: Arbitrary[F[B]]
  ): Arbitrary[Kleisli[F, A, B]] =
    Arbitrary(Arbitrary.arbitrary[A => F[B]].map(Kleisli(_)))

  implicit def catsLawsArbitraryForCokleisli[F[_], A, B](implicit
    AFA: Arbitrary[F[A]],
    CFA: Cogen[F[A]],
    B: Arbitrary[B]
  ): Arbitrary[Cokleisli[F, A, B]] =
    Arbitrary(Arbitrary.arbitrary[F[A] => B].map(Cokleisli(_)))
}
