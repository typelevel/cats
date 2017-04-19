package cats
package laws
package discipline

import scala.util.{Try, Success, Failure}

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

  implicit def catsLawsArbitraryForNonEmptyList[A](implicit A: Arbitrary[A]): Arbitrary[NonEmptyList[A]] =
    Arbitrary(implicitly[Arbitrary[List[A]]].arbitrary.flatMap(fa => A.arbitrary.map(a => NonEmptyList(a, fa))))

  implicit def catsLawsCogenForNonEmptyList[A](implicit A: Cogen[A]): Cogen[NonEmptyList[A]] =
    Cogen[List[A]].contramap(_.toList)

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


  implicit def catsLawsArbitraryForCokleisli[F[_], A, B](implicit B: Arbitrary[B]): Arbitrary[Cokleisli[F, A, B]] =
    Arbitrary(B.arbitrary.map(b => Cokleisli[F, A, B](_ => b)))

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
      getArbitrary[A].map(Eval.now(_)),
      getArbitrary[A].map(Eval.later(_)),
      getArbitrary[A].map(Eval.always(_))))

  implicit def catsLawsCogenForEval[A: Cogen]: Cogen[Eval[A]] =
    Cogen[A].contramap(_.value)

  implicit def catsLawsArbitraryForTuple2K[F[_], G[_], A](implicit F: Arbitrary[F[A]], G: Arbitrary[G[A]]): Arbitrary[Tuple2K[F, G, A]] =
    Arbitrary(F.arbitrary.flatMap(fa => G.arbitrary.map(ga => Tuple2K[F, G, A](fa, ga))))

  implicit def catsLawsArbitraryForFunc[F[_], A, B](implicit F: Arbitrary[F[B]]): Arbitrary[Func[F, A, B]] =
    Arbitrary(F.arbitrary.map(fb => Func.func[F, A, B](_ => fb)))

  implicit def catsLawsArbitraryForAppFunc[F[_], A, B](implicit F: Arbitrary[F[B]], FF: Applicative[F]): Arbitrary[AppFunc[F, A, B]] =
    Arbitrary(F.arbitrary.map(fb => Func.appFunc[F, A, B](_ => fb)))

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

  implicit def catsLawsArbitraryForEq[A: Arbitrary]: Arbitrary[Eq[A]] =
    Arbitrary(new Eq[A] { def eqv(x: A, y: A) = x.hashCode == y.hashCode })

  implicit def catsLawsArbitraryForPartialOrder[A: Arbitrary]: Arbitrary[PartialOrder[A]] =
    Arbitrary(Gen.oneOf(
      PartialOrder.from[A]((_: A, _: A) => Double.NaN),
      PartialOrder.from[A]((_: A, _: A) => -1.0),
      PartialOrder.from[A]((_: A, _: A) => 0.0),
      PartialOrder.from[A]((_: A, _: A) => 1.0)))

  implicit def catsLawsArbitraryForOrder[A: Arbitrary]: Arbitrary[Order[A]] =
    Arbitrary(Gen.oneOf(
      Order.from[A]((_: A, _: A) => -1),
      Order.from[A]((_: A, _: A) => 0),
      Order.from[A]((_: A, _: A) => 1)))

  implicit def catsLawsArbitraryForNested[F[_], G[_], A](implicit FG: Arbitrary[F[G[A]]]): Arbitrary[Nested[F, G, A]] =
    Arbitrary(FG.arbitrary.map(Nested(_)))

  implicit def catsLawArbitraryForState[S: Arbitrary: Cogen, A: Arbitrary]: Arbitrary[State[S, A]] =
    catsLawArbitraryForStateT[Eval, S, A]

  implicit def catsLawArbitraryForReader[A: Arbitrary, B: Arbitrary]: Arbitrary[Reader[A, B]] =
    catsLawsArbitraryForKleisli[Id, A, B]
}

private[discipline] sealed trait ArbitraryInstances0 {

  implicit def catsLawArbitraryForStateT[F[_]: Applicative, S, A](implicit F: Arbitrary[S => F[(S, A)]]): Arbitrary[StateT[F, S, A]] =
    Arbitrary(F.arbitrary.map(f => StateT(f)))

  implicit def catsLawsArbitraryForWriterT[F[_], L, V](implicit F: Arbitrary[F[(L, V)]]): Arbitrary[WriterT[F, L, V]] =
    Arbitrary(F.arbitrary.map(WriterT(_)))

  implicit def catsLawsCogenForWriterT[F[_], L, V](implicit F: Cogen[F[(L, V)]]): Cogen[WriterT[F, L, V]] =
    F.contramap(_.run)

  implicit def catsLawsArbitraryForKleisli[F[_], A, B](implicit F: Arbitrary[F[B]]): Arbitrary[Kleisli[F, A, B]] =
    Arbitrary(F.arbitrary.map(fb => Kleisli[F, A, B](_ => fb)))

}
