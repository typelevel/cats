package cats
package laws
package discipline

import cats.data._
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary.{arbitrary => getArbitrary}

/**
 * Arbitrary instances for cats.data
 */
object arbitrary {

  implicit def constArbitrary[A, B](implicit A: Arbitrary[A]): Arbitrary[Const[A, B]] =
    Arbitrary(A.arbitrary.map(Const[A, B]))

  implicit def oneAndArbitrary[F[_], A](implicit A: Arbitrary[A], F: ArbitraryK[F]): Arbitrary[OneAnd[A, F]] =
    Arbitrary(F.synthesize[A].arbitrary.flatMap(fa => A.arbitrary.map(a => OneAnd(a, fa))))

  implicit def xorArbitrary[A, B](implicit A: Arbitrary[A], B: Arbitrary[B]): Arbitrary[A Xor B] =
    Arbitrary(Gen.oneOf(A.arbitrary.map(Xor.left), B.arbitrary.map(Xor.right)))

  implicit def xorTArbitrary[F[_], A, B](implicit F: ArbitraryK[F], A: Arbitrary[A], B: Arbitrary[B]): Arbitrary[XorT[F, A, B]] =
    Arbitrary(F.synthesize[A Xor B].arbitrary.map(XorT(_)))

  implicit def validatedArbitrary[A, B](implicit A: Arbitrary[A], B: Arbitrary[B]): Arbitrary[Validated[A, B]] =
    Arbitrary(Gen.oneOf(A.arbitrary.map(Validated.invalid), B.arbitrary.map(Validated.valid)))

  implicit def iorArbitrary[A, B](implicit A: Arbitrary[A], B: Arbitrary[B]): Arbitrary[A Ior B] =
    Arbitrary(Gen.oneOf(A.arbitrary.map(Ior.left), B.arbitrary.map(Ior.right), for { a <- A.arbitrary; b <- B.arbitrary } yield Ior.both(a, b)))

  implicit def kleisliArbitrary[F[_], A, B](implicit F: ArbitraryK[F], B: Arbitrary[B]): Arbitrary[Kleisli[F, A, B]] =
    Arbitrary(F.synthesize[B].arbitrary.map(fb => Kleisli[F, A, B](_ => fb)))

  implicit def cokleisliArbitrary[F[_], A, B](implicit B: Arbitrary[B]): Arbitrary[Cokleisli[F, A, B]] =
    Arbitrary(B.arbitrary.map(b => Cokleisli[F, A, B](_ => b)))

  implicit def optionTArbitrary[F[_], A](implicit F: ArbitraryK[F], A: Arbitrary[A]): Arbitrary[OptionT[F, A]] =
    Arbitrary(F.synthesize[Option[A]].arbitrary.map(OptionT.apply))

  implicit def evalArbitrary[A: Arbitrary]: Arbitrary[Eval[A]] =
    Arbitrary(Gen.oneOf(
      getArbitrary[A].map(Eval.now(_)),
      getArbitrary[A].map(Eval.later(_)),
      getArbitrary[A].map(Eval.always(_))))

  implicit def prodArbitrary[F[_], G[_], A](implicit F: ArbitraryK[F], G: ArbitraryK[G], A: Arbitrary[A]): Arbitrary[Prod[F, G, A]] =
    Arbitrary(F.synthesize[A].arbitrary.flatMap(fa => G.synthesize[A].arbitrary.map(ga =>  Prod[F, G, A](fa, ga))))

  implicit def funcArbitrary[F[_], A, B](implicit F: ArbitraryK[F], B: Arbitrary[B]): Arbitrary[Func[F, A, B]] =
    Arbitrary(F.synthesize[B].arbitrary.map(fb => Func.func[F, A, B](_ => fb)))

  implicit def appFuncArbitrary[F[_], A, B](implicit F: ArbitraryK[F], B: Arbitrary[B], FF: Applicative[F]): Arbitrary[AppFunc[F, A, B]] =
    Arbitrary(F.synthesize[B].arbitrary.map(fb => Func.appFunc[F, A, B](_ => fb)))

  implicit def streamingArbitrary[A](implicit A: Arbitrary[A]): Arbitrary[Streaming[A]] =
    Arbitrary(Gen.listOf(A.arbitrary).map(Streaming.fromList(_)))

  // TODO: it would be better to do this recursively, i.e. more like:
  //
  // Gen.oneOf(
  //   for { a <- arbitrary[A]; s <- arbitrary[F[StreamingT[F, A]]] } yield cons(a, s),
  //   for { s <- arbitrary[F[StreamingT[F, A]]] } yield wait(s),
  //   const(StreamingT.empty[F, A]))
  //
  // However, getting this right with Scalacheck (and avoiding SOEs) is
  // somewhat fiddly, so this will have to do for now.
  //
  // The max possible size of a StreamingT instance (n) will result in
  // instances of up to n^3 in length when testing flatMap
  // composition. The current value (8) could result in streams of up
  // to 512 elements in length. Thus, since F may not be stack-safe,
  // we want to keep n relatively small.
  implicit def streamingTArbitrary[F[_], A](implicit F: Monad[F], A: Arbitrary[A]): Arbitrary[StreamingT[F, A]] =
    Arbitrary(for {
      as <- Gen.listOf(A.arbitrary).map(_.take(8))
    } yield StreamingT.fromList(as))
}
