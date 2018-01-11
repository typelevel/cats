package cats
package laws
package discipline

import cats.data.NonEmptyList.ZipNonEmptyList
import cats.data.NonEmptyVector.ZipNonEmptyVector
import scala.util.{Failure, Success, Try}
import scala.collection.immutable.{SortedMap, SortedSet}
import cats.data._
import org.scalacheck.{Arbitrary, Cogen, Gen}
import org.scalacheck.Arbitrary.{arbitrary => getArbitrary}

/**
 * Arbitrary instances for cats.data
 */
object arbitrary extends ArbitraryInstances0 {

  // this instance is not available in scalacheck 1.13.2.
  // remove this once a newer version is available.
  implicit val catsLawsCogenForThrowable: Cogen[Throwable] =
    Cogen[String].contramap(_.toString)

  // this instance is not available in scalacheck 1.13.2.
  // remove this once a newer version is available.
  implicit def catsLawsCogenForTry[A](implicit A: Cogen[A]): Cogen[Try[A]] =
    Cogen((seed, x) => x match {
      case Success(a) => A.perturb(seed, a)
      case Failure(e) => Cogen[Throwable].perturb(seed, e)
    })

  // this instance is not available in scalacheck 1.13.2.
  // remove this once a newer version is available.
  implicit def catsLawsCogenForFunction0[A](implicit A: Cogen[A]): Cogen[Function0[A]] =
    A.contramap(_())

  implicit def catsLawsArbitraryForConst[A, B](implicit A: Arbitrary[A]): Arbitrary[Const[A, B]] =
    Arbitrary(A.arbitrary.map(Const[A, B]))

  implicit def catsLawsCogenForConst[A, B](implicit A: Cogen[A]): Cogen[Const[A, B]] =
    A.contramap(_.getConst)

  implicit def catsLawsArbitraryForOneAnd[F[_], A](implicit A: Arbitrary[A], F: Arbitrary[F[A]]): Arbitrary[OneAnd[F, A]] =
    Arbitrary(F.arbitrary.flatMap(fa => A.arbitrary.map(a => OneAnd(a, fa))))

  implicit def catsLawsCogenForOneAnd[F[_], A](implicit A: Cogen[A], F: Cogen[F[A]]): Cogen[OneAnd[F, A]] =
    Cogen((seed, x) => F.perturb(A.perturb(seed, x.head), x.tail))

  implicit def catsLawsArbitraryForNonEmptyVector[A](implicit A: Arbitrary[A]): Arbitrary[NonEmptyVector[A]] =
    Arbitrary(implicitly[Arbitrary[Vector[A]]].arbitrary.flatMap(fa => A.arbitrary.map(a => NonEmptyVector(a, fa))))

  implicit def catsLawsCogenForNonEmptyVector[A](implicit A: Cogen[A]): Cogen[NonEmptyVector[A]] =
    Cogen[Vector[A]].contramap(_.toVector)

  implicit def catsLawsArbitraryForZipVector[A](implicit A: Arbitrary[A]): Arbitrary[ZipVector[A]] =
    Arbitrary(implicitly[Arbitrary[Vector[A]]].arbitrary.map(v => new ZipVector(v)))

  implicit def catsLawsArbitraryForZipList[A](implicit A: Arbitrary[A]): Arbitrary[ZipList[A]] =
    Arbitrary(implicitly[Arbitrary[List[A]]].arbitrary.map(v => new ZipList(v)))

  implicit def catsLawsArbitraryForZipStream[A](implicit A: Arbitrary[A]): Arbitrary[ZipStream[A]] =
    Arbitrary(implicitly[Arbitrary[Stream[A]]].arbitrary.map(v => new ZipStream(v)))

  implicit def catsLawsArbitraryForZipNonEmptyVector[A](implicit A: Arbitrary[A]): Arbitrary[ZipNonEmptyVector[A]] =
    Arbitrary(implicitly[Arbitrary[NonEmptyVector[A]]].arbitrary.map(nev => new ZipNonEmptyVector(nev)))

  implicit def catsLawsArbitraryForNonEmptyList[A](implicit A: Arbitrary[A]): Arbitrary[NonEmptyList[A]] =
    Arbitrary(implicitly[Arbitrary[List[A]]].arbitrary.flatMap(fa => A.arbitrary.map(a => NonEmptyList(a, fa))))

  implicit def catsLawsCogenForNonEmptyList[A](implicit A: Cogen[A]): Cogen[NonEmptyList[A]] =
    Cogen[List[A]].contramap(_.toList)


  implicit def catsLawsArbitraryForZipNonEmptyList[A](implicit A: Arbitrary[A]): Arbitrary[ZipNonEmptyList[A]] =
    Arbitrary(implicitly[Arbitrary[NonEmptyList[A]]].arbitrary.map(nel => new ZipNonEmptyList(nel)))

  implicit def catsLawsArbitraryForEitherT[F[_], A, B](implicit F: Arbitrary[F[Either[A, B]]]): Arbitrary[EitherT[F, A, B]] =
    Arbitrary(F.arbitrary.map(EitherT(_)))

  implicit def catsLawsCogenForEitherT[F[_], A, B](implicit F: Cogen[F[Either[A, B]]]): Cogen[EitherT[F, A, B]] =
    F.contramap(_.value)

  implicit def catsLawsArbitraryForValidated[A, B](implicit A: Arbitrary[A], B: Arbitrary[B]): Arbitrary[Validated[A, B]] =
    Arbitrary(Gen.oneOf(A.arbitrary.map(Validated.invalid), B.arbitrary.map(Validated.valid)))

  implicit def catsLawsCogenForValidated[A, B](implicit A: Cogen[A], B: Cogen[B]): Cogen[Validated[A, B]] =
    Cogen((seed, x) => x.fold(A.perturb(seed, _), B.perturb(seed, _)))

  implicit def catsLawsArbitraryForIor[A, B](implicit A: Arbitrary[A], B: Arbitrary[B]): Arbitrary[A Ior B] =
    Arbitrary(Gen.oneOf(A.arbitrary.map(Ior.left), B.arbitrary.map(Ior.right), for { a <- A.arbitrary; b <- B.arbitrary } yield Ior.both(a, b)))

  implicit def catsLawsCogenForIor[A, B](implicit A: Cogen[A], B: Cogen[B]): Cogen[A Ior B] =
    Cogen((seed, x) => x.fold(
      A.perturb(seed, _),
      B.perturb(seed, _),
      (a, b) => A.perturb(B.perturb(seed, b), a)))

  implicit def catsLawsArbitraryForIorT[F[_], A, B](implicit F: Arbitrary[F[Ior[A, B]]]): Arbitrary[IorT[F, A, B]] =
    Arbitrary(F.arbitrary.map(IorT(_)))

  implicit def catsLawsCogenForIorT[F[_], A, B](implicit F: Cogen[F[Ior[A, B]]]): Cogen[IorT[F, A, B]] =
    F.contramap(_.value)

  implicit def catsLawsArbitraryForOptionT[F[_], A](implicit F: Arbitrary[F[Option[A]]]): Arbitrary[OptionT[F, A]] =
    Arbitrary(F.arbitrary.map(OptionT.apply))

  implicit def catsLawsCogenForOptionT[F[_], A](implicit F: Cogen[F[Option[A]]]): Cogen[OptionT[F, A]] =
    F.contramap(_.value)

  implicit def catsLawsArbitraryForIdT[F[_], A](implicit F: Arbitrary[F[A]]): Arbitrary[IdT[F, A]] =
    Arbitrary(F.arbitrary.map(IdT.apply))

  implicit def catsLawsCogenForIdT[F[_], A](implicit F: Cogen[F[A]]): Cogen[IdT[F, A]] =
    F.contramap(_.value)

  implicit def catsLawsArbitraryForEval[A: Arbitrary]: Arbitrary[Eval[A]] =
    Arbitrary(Gen.oneOf(
      getArbitrary[A].map(a => Eval.now(a)),
      getArbitrary[() => A].map(f => Eval.later(f())),
      getArbitrary[() => A].map(f => Eval.always(f()))))

  implicit def catsLawsCogenForEval[A: Cogen]: Cogen[Eval[A]] =
    Cogen[A].contramap(_.value)

  implicit def catsLawsArbitraryForTuple2K[F[_], G[_], A](implicit F: Arbitrary[F[A]], G: Arbitrary[G[A]]): Arbitrary[Tuple2K[F, G, A]] =
    Arbitrary(F.arbitrary.flatMap(fa => G.arbitrary.map(ga => Tuple2K[F, G, A](fa, ga))))

  implicit def catsLawsArbitraryForFunc[F[_], A, B](implicit AA: Arbitrary[A], CA: Cogen[A], F: Arbitrary[F[B]]): Arbitrary[Func[F, A, B]] =
    Arbitrary(Arbitrary.arbitrary[A => F[B]].map(Func.func))

  implicit def catsLawsArbitraryForAppFunc[F[_], A, B](implicit AA: Arbitrary[A], CA: Cogen[A], F: Arbitrary[F[B]], FF: Applicative[F]): Arbitrary[AppFunc[F, A, B]] =
    Arbitrary(Arbitrary.arbitrary[A => F[B]].map(Func.appFunc(_)))

  implicit def catsLawsArbitraryForWriter[L:Arbitrary, V:Arbitrary]: Arbitrary[Writer[L, V]] =
    catsLawsArbitraryForWriterT[Id, L, V]

  implicit def catsLawsCogenForWriter[L: Cogen, V: Cogen]: Cogen[Writer[L, V]] =
    Cogen[(L, V)].contramap(_.run)

  // until this is provided by scalacheck
  implicit def catsLawsArbitraryForPartialFunction[A, B](implicit F: Arbitrary[A => Option[B]]): Arbitrary[PartialFunction[A, B]] =
    Arbitrary(F.arbitrary.map(Function.unlift))

  implicit def catsLawsArbitraryForEitherK[F[_], G[_], A](implicit F: Arbitrary[F[A]], G: Arbitrary[G[A]]): Arbitrary[EitherK[F, G, A]] =
    Arbitrary(Gen.oneOf(
      F.arbitrary.map(EitherK.leftc[F, G, A]),
      G.arbitrary.map(EitherK.rightc[F, G, A])))

  implicit def catsLawsCogenForEitherK[F[_], G[_], A](implicit F: Cogen[F[A]], G: Cogen[G[A]]): Cogen[EitherK[F, G, A]] =
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
    Arbitrary(getArbitrary[Int => Int].map(f => new Eq[A] {
      def eqv(x: A, y: A): Boolean = f(x.##) == f(y.##)
    }))

  implicit def catsLawsArbitraryForEquiv[A: Arbitrary]: Arbitrary[Equiv[A]] =
    Arbitrary(getArbitrary[Eq[A]].map(Eq.catsKernelEquivForEq(_)))

  implicit def catsLawsArbitraryForPartialOrder[A: Arbitrary]: Arbitrary[PartialOrder[A]] =
    Arbitrary(getArbitrary[Int => Double].map(f => new PartialOrder[A] {
      def partialCompare(x: A, y: A): Double =
        if (x.## == y.##) 0.0 else f(x.##) - f(y.##)
    }))

  implicit def catsLawsArbitraryForPartialOrdering[A: Arbitrary]: Arbitrary[PartialOrdering[A]] =
    Arbitrary(getArbitrary[PartialOrder[A]].map(PartialOrder.catsKernelPartialOrderingForPartialOrder(_)))

  implicit def catsLawsArbitraryForOrder[A: Arbitrary]: Arbitrary[Order[A]] =
    Arbitrary(getArbitrary[Int => Int].map(f => new Order[A] {
      def compare(x: A, y: A): Int = java.lang.Integer.compare(f(x.##), f(y.##))
    }))

  implicit def catsLawsArbitraryForSortedMap[K: Arbitrary: Order, V: Arbitrary]: Arbitrary[SortedMap[K, V]] =
    Arbitrary(getArbitrary[Map[K, V]].map(s => SortedMap.empty[K, V](implicitly[Order[K]].toOrdering) ++ s))

  implicit def catsLawsCogenForSortedMap[K: Order: Cogen, V: Order: Cogen]: Cogen[SortedMap[K, V]] = {
    implicit val orderingK = Order[K].toOrdering
    implicit val orderingV = Order[V].toOrdering

    implicitly[Cogen[Map[K, V]]].contramap(_.toMap)
  }

  implicit def catsLawsArbitraryForSortedSet[A: Arbitrary: Order]: Arbitrary[SortedSet[A]] =
    Arbitrary(getArbitrary[Set[A]].map(s => SortedSet.empty[A](implicitly[Order[A]].toOrdering) ++ s))

  implicit def catsLawsCogenForSortedSet[A: Order: Cogen]: Cogen[SortedSet[A]] = {
    implicit val orderingA = Order[A].toOrdering

    implicitly[Cogen[Set[A]]].contramap(_.toSet)
  }

  implicit def catsLawsArbitraryForOrdering[A: Arbitrary]: Arbitrary[Ordering[A]] =
    Arbitrary(getArbitrary[Order[A]].map(Order.catsKernelOrderingForOrder(_)))

  implicit def catsLawsArbitraryForHash[A: Hash]: Arbitrary[Hash[A]] =
    Arbitrary(Hash.fromUniversalHashCode[A])

  implicit def catsLawsArbitraryForNested[F[_], G[_], A](implicit FG: Arbitrary[F[G[A]]]): Arbitrary[Nested[F, G, A]] =
    Arbitrary(FG.arbitrary.map(Nested(_)))

  implicit def catsLawArbitraryForState[S: Arbitrary: Cogen, A: Arbitrary]: Arbitrary[State[S, A]] =
    catsLawArbitraryForIndexedStateT[Eval, S, S, A]

  implicit def catsLawArbitraryForReader[A: Arbitrary: Cogen, B: Arbitrary]: Arbitrary[Reader[A, B]] =
    catsLawsArbitraryForKleisli[Id, A, B]

  implicit def catsLawArbitraryForCokleisliId[A: Arbitrary: Cogen, B: Arbitrary]: Arbitrary[Cokleisli[Id, A, B]] =
    catsLawsArbitraryForCokleisli[Id, A, B]

  implicit def catsLawsArbitraryForIRWST[F[_]: Applicative, E, L, SA, SB, A](implicit
    F: Arbitrary[(E, SA) => F[(L, SB, A)]]): Arbitrary[IndexedReaderWriterStateT[F, E, L, SA, SB, A]] =
    Arbitrary(F.arbitrary.map(IndexedReaderWriterStateT(_)))

}

private[discipline] sealed trait ArbitraryInstances0 {

  implicit def catsLawArbitraryForIndexedStateT[F[_], SA, SB, A](implicit F: Arbitrary[F[SA => F[(SB, A)]]]): Arbitrary[IndexedStateT[F, SA, SB, A]] =
    Arbitrary(F.arbitrary.map(IndexedStateT.applyF))

  implicit def catsLawsArbitraryForWriterT[F[_], L, V](implicit F: Arbitrary[F[(L, V)]]): Arbitrary[WriterT[F, L, V]] =
    Arbitrary(F.arbitrary.map(WriterT(_)))

  implicit def catsLawsCogenForWriterT[F[_], L, V](implicit F: Cogen[F[(L, V)]]): Cogen[WriterT[F, L, V]] =
    F.contramap(_.run)

  implicit def catsLawsArbitraryForKleisli[F[_], A, B](implicit AA: Arbitrary[A], CA: Cogen[A], F: Arbitrary[F[B]]): Arbitrary[Kleisli[F, A, B]] =
    Arbitrary(Arbitrary.arbitrary[A => F[B]].map(Kleisli(_)))

  implicit def catsLawsArbitraryForCokleisli[F[_], A, B](implicit AFA: Arbitrary[F[A]], CFA: Cogen[F[A]], B: Arbitrary[B]): Arbitrary[Cokleisli[F, A, B]] =
    Arbitrary(Arbitrary.arbitrary[F[A] => B].map(Cokleisli(_)))
}
