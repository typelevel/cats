package cats
package laws
package discipline

import cats.data._
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary.{arbitrary => getArbitrary}

/**
 * Arbitrary instances for cats.data
 */
object arbitrary extends ArbitraryInstances0 {

  // A special function1Arbitrary for testing operations like dropWhile specifically
  // in the context of Int => Boolean. Once scalacheck supports better Function1 arbitrary
  // instances this can be removed.
  implicit def catsLawsArbitraryForIntToBool: Arbitrary[(Int) => Boolean] =
    Arbitrary(
      getArbitrary[Int].map(x =>
       new Function1[Int, Boolean] {
         def apply(i: Int): Boolean = i < x

         override def toString = s"<function testing whether input is less than $x>"
       }))

  implicit def catsLawsArbitraryForConst[A, B](implicit A: Arbitrary[A]): Arbitrary[Const[A, B]] =
    Arbitrary(A.arbitrary.map(Const[A, B]))

  implicit def catsLawsArbitraryForOneAnd[F[_], A](implicit A: Arbitrary[A], F: Arbitrary[F[A]]): Arbitrary[OneAnd[F, A]] =
    Arbitrary(F.arbitrary.flatMap(fa => A.arbitrary.map(a => OneAnd(a, fa))))

  implicit def catsLawsArbitraryForNonEmptyVector[A](implicit A: Arbitrary[A]): Arbitrary[NonEmptyVector[A]] =
    Arbitrary(implicitly[Arbitrary[Vector[A]]].arbitrary.flatMap(fa => A.arbitrary.map(a => NonEmptyVector(a, fa))))

  implicit def catsLawsArbitraryForNonEmptyList[A](implicit A: Arbitrary[A]): Arbitrary[NonEmptyList[A]] =
    Arbitrary(implicitly[Arbitrary[List[A]]].arbitrary.flatMap(fa => A.arbitrary.map(a => NonEmptyList(a, fa))))

  implicit def catsLawsArbitraryForXor[A, B](implicit A: Arbitrary[A], B: Arbitrary[B]): Arbitrary[A Xor B] =
    Arbitrary(Gen.oneOf(A.arbitrary.map(Xor.left), B.arbitrary.map(Xor.right)))

  implicit def catsLawsArbitraryForXorT[F[_], A, B](implicit F: Arbitrary[F[A Xor B]]): Arbitrary[XorT[F, A, B]] =
    Arbitrary(F.arbitrary.map(XorT(_)))

  implicit def catsLawsArbitraryForEitherT[F[_], A, B](implicit F: Arbitrary[F[Either[A, B]]]): Arbitrary[EitherT[F, A, B]] =
    Arbitrary(F.arbitrary.map(EitherT(_)))

  implicit def catsLawsArbitraryForValidated[A, B](implicit A: Arbitrary[A], B: Arbitrary[B]): Arbitrary[Validated[A, B]] =
    Arbitrary(Gen.oneOf(A.arbitrary.map(Validated.invalid), B.arbitrary.map(Validated.valid)))

  implicit def catsLawsArbitraryForIor[A, B](implicit A: Arbitrary[A], B: Arbitrary[B]): Arbitrary[A Ior B] =
    Arbitrary(Gen.oneOf(A.arbitrary.map(Ior.left), B.arbitrary.map(Ior.right), for { a <- A.arbitrary; b <- B.arbitrary } yield Ior.both(a, b)))

  implicit def catsLawsArbitraryForKleisli[F[_], A, B](implicit F: Arbitrary[F[B]]): Arbitrary[Kleisli[F, A, B]] =
    Arbitrary(F.arbitrary.map(fb => Kleisli[F, A, B](_ => fb)))

  implicit def catsLawsArbitraryForCokleisli[F[_], A, B](implicit B: Arbitrary[B]): Arbitrary[Cokleisli[F, A, B]] =
    Arbitrary(B.arbitrary.map(b => Cokleisli[F, A, B](_ => b)))

  implicit def catsLawsArbitraryForOptionT[F[_], A](implicit F: Arbitrary[F[Option[A]]]): Arbitrary[OptionT[F, A]] =
    Arbitrary(F.arbitrary.map(OptionT.apply))

  implicit def catsLawsArbitraryForIdT[F[_], A](implicit F: Arbitrary[F[A]]): Arbitrary[IdT[F, A]] =
    Arbitrary(F.arbitrary.map(IdT.apply))

  implicit def catsLawsArbitraryForEval[A: Arbitrary]: Arbitrary[Eval[A]] =
    Arbitrary(Gen.oneOf(
      getArbitrary[A].map(Eval.now(_)),
      getArbitrary[A].map(Eval.later(_)),
      getArbitrary[A].map(Eval.always(_))))

  implicit def catsLawsArbitraryForProd[F[_], G[_], A](implicit F: Arbitrary[F[A]], G: Arbitrary[G[A]]): Arbitrary[Prod[F, G, A]] =
    Arbitrary(F.arbitrary.flatMap(fa => G.arbitrary.map(ga => Prod[F, G, A](fa, ga))))

  implicit def catsLawsArbitraryForFunc[F[_], A, B](implicit F: Arbitrary[F[B]]): Arbitrary[Func[F, A, B]] =
    Arbitrary(F.arbitrary.map(fb => Func.func[F, A, B](_ => fb)))

  implicit def catsLawsArbitraryForAppFunc[F[_], A, B](implicit F: Arbitrary[F[B]], FF: Applicative[F]): Arbitrary[AppFunc[F, A, B]] =
    Arbitrary(F.arbitrary.map(fb => Func.appFunc[F, A, B](_ => fb)))

  implicit def catsLawsArbitraryForWriter[L:Arbitrary, V:Arbitrary]: Arbitrary[Writer[L, V]] =
    catsLawsArbitraryForWriterT[Id, L, V]

  // until this is provided by scalacheck
  implicit def catsLawsArbitraryForPartialFunction[A, B](implicit F: Arbitrary[A => Option[B]]): Arbitrary[PartialFunction[A, B]] =
    Arbitrary(F.arbitrary.map(Function.unlift))

  implicit def catsLawsArbitraryForCoproduct[F[_], G[_], A](implicit F: Arbitrary[F[A]], G: Arbitrary[G[A]]): Arbitrary[Coproduct[F, G, A]] =
    Arbitrary(Gen.oneOf(
      F.arbitrary.map(Coproduct.leftc[F, G, A]),
      G.arbitrary.map(Coproduct.rightc[F, G, A])))

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
}

private[discipline] sealed trait ArbitraryInstances0 {
  implicit def catsLawsArbitraryForWriterT[F[_], L, V](implicit F: Arbitrary[F[(L, V)]]): Arbitrary[WriterT[F, L, V]] =
    Arbitrary(F.arbitrary.map(WriterT(_)))
}
