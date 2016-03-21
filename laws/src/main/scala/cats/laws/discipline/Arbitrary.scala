package cats
package laws
package discipline

import cats.data._
import org.scalacheck.{Arbitrary, Cogen, Gen}
import org.scalacheck.Arbitrary.{arbitrary => getArbitrary}

/**
 * Arbitrary instances for cats.data
 */
object arbitrary extends CogenInstances with ArbitraryInstances0 {
  implicit def constArbitrary[A, B](implicit A: Arbitrary[A]): Arbitrary[Const[A, B]] =
    Arbitrary(A.arbitrary.map(Const[A, B]))

  implicit def oneAndArbitrary[F[_], A](implicit A: Arbitrary[A], F: Arbitrary[F[A]]): Arbitrary[OneAnd[F, A]] =
    Arbitrary(F.arbitrary.flatMap(fa => A.arbitrary.map(a => OneAnd(a, fa))))

  implicit def xorArbitrary[A, B](implicit A: Arbitrary[A], B: Arbitrary[B]): Arbitrary[A Xor B] =
    Arbitrary(Gen.oneOf(A.arbitrary.map(Xor.left), B.arbitrary.map(Xor.right)))

  implicit def xorTArbitrary[F[_], A, B](implicit F: Arbitrary[F[A Xor B]]): Arbitrary[XorT[F, A, B]] =
    Arbitrary(F.arbitrary.map(XorT(_)))

  implicit def validatedArbitrary[A, B](implicit A: Arbitrary[A], B: Arbitrary[B]): Arbitrary[Validated[A, B]] =
    Arbitrary(Gen.oneOf(A.arbitrary.map(Validated.invalid), B.arbitrary.map(Validated.valid)))

  implicit def iorArbitrary[A, B](implicit A: Arbitrary[A], B: Arbitrary[B]): Arbitrary[A Ior B] =
    Arbitrary(Gen.oneOf(A.arbitrary.map(Ior.left), B.arbitrary.map(Ior.right), for { a <- A.arbitrary; b <- B.arbitrary } yield Ior.both(a, b)))

  implicit def kleisliArbitrary[F[_], A, B](implicit F: Arbitrary[F[B]]): Arbitrary[Kleisli[F, A, B]] =
    Arbitrary(F.arbitrary.map(fb => Kleisli[F, A, B](_ => fb)))

  implicit def cokleisliArbitrary[F[_], A, B](implicit B: Arbitrary[B]): Arbitrary[Cokleisli[F, A, B]] =
    Arbitrary(B.arbitrary.map(b => Cokleisli[F, A, B](_ => b)))

  implicit def optionTArbitrary[F[_], A](implicit F: Arbitrary[F[Option[A]]]): Arbitrary[OptionT[F, A]] =
    Arbitrary(F.arbitrary.map(OptionT.apply))

  implicit def evalArbitrary[A: Arbitrary]: Arbitrary[Eval[A]] =
    Arbitrary(Gen.oneOf(
      getArbitrary[A].map(Eval.now(_)),
      getArbitrary[A].map(Eval.later(_)),
      getArbitrary[A].map(Eval.always(_))))

  implicit def prodArbitrary[F[_], G[_], A](implicit F: Arbitrary[F[A]], G: Arbitrary[G[A]]): Arbitrary[Prod[F, G, A]] =
    Arbitrary(F.arbitrary.flatMap(fa => G.arbitrary.map(ga => Prod[F, G, A](fa, ga))))

  implicit def funcArbitrary[F[_], A, B](implicit F: Arbitrary[F[B]]): Arbitrary[Func[F, A, B]] =
    Arbitrary(F.arbitrary.map(fb => Func.func[F, A, B](_ => fb)))

  implicit def appFuncArbitrary[F[_], A, B](implicit F: Arbitrary[F[B]], FF: Applicative[F]): Arbitrary[AppFunc[F, A, B]] =
    Arbitrary(F.arbitrary.map(fb => Func.appFunc[F, A, B](_ => fb)))

  implicit def writerArbitrary[L:Arbitrary, V:Arbitrary]: Arbitrary[Writer[L, V]] =
    writerTArbitrary[Id, L, V]

  // until this is provided by scalacheck
  implicit def partialFunctionArbitrary[A, B](implicit F: Arbitrary[A => Option[B]]): Arbitrary[PartialFunction[A, B]] =
    Arbitrary(F.arbitrary.map(Function.unlift))

  implicit def coproductArbitrary[F[_], G[_], A](implicit F: Arbitrary[F[A]], G: Arbitrary[G[A]]): Arbitrary[Coproduct[F, G, A]] =
    Arbitrary(Gen.oneOf(
      F.arbitrary.map(Coproduct.leftc[F, G, A]),
      G.arbitrary.map(Coproduct.rightc[F, G, A])))

  implicit def showArbitrary[A: Arbitrary]: Arbitrary[Show[A]] =
    Arbitrary(Show.fromToString[A])

  implicit def function0Arbitrary[A: Arbitrary]: Arbitrary[() => A] =
    Arbitrary(getArbitrary[A].map(() => _))
}

private[discipline] sealed trait ArbitraryInstances0 {
  implicit def writerTArbitrary[F[_], L, V](implicit F: Arbitrary[F[(L, V)]]): Arbitrary[WriterT[F, L, V]] =
    Arbitrary(F.arbitrary.map(WriterT(_)))
}

private[discipline] sealed trait CogenInstances extends StdLibCogen {
  implicit def coproductCogen[F[_], G[_], A](implicit
    CogenFA: Cogen[F[A]],
    CogenGA: Cogen[G[A]]
  ): Cogen[Coproduct[F, G, A]] =
    Cogen[Xor[F[A], G[A]]].contramap(_.run)

  implicit def evalCogen[A: Cogen]: Cogen[Eval[A]] =
    Cogen((seed, evalA) => Cogen[A].perturb(seed, evalA.value))

  implicit def oneAndCogen[F[_], A](implicit FA: Cogen[(A, F[A])]): Cogen[OneAnd[F, A]] =
    FA.contramap(o => (o.head, o.tail))

  implicit def xorCogen[A: Cogen, B: Cogen]: Cogen[Xor[A, B]] =
    Cogen[Either[A, B]].contramap(_.toEither)
}

private[discipline] sealed trait StdLibCogen {
  implicit def function0Cogen[A: Cogen]: Cogen[Function0[A]] =
    Cogen[A].contramap(_())

  implicit def streamCogen[A: Cogen]: Cogen[Stream[A]] =
    Cogen[List[A]].contramap(_.toList)
}
