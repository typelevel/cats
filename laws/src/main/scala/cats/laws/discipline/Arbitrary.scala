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
  implicit def function1Arbitrary: Arbitrary[(Int) => Boolean] =
    Arbitrary(getArbitrary[Int].map(x => (input) => input < x))

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

  def streamingGen[A:Arbitrary](maxDepth: Int): Gen[Streaming[A]] =
    if (maxDepth <= 1)
      Gen.const(Streaming.empty[A])
    else {
      // the arbitrary instance for the next layer of the stream
      implicit val A = Arbitrary(streamingGen[A](maxDepth - 1))
      Gen.frequency(
        // Empty
        1 -> Gen.const(Streaming.empty[A]),
        // Wait
        2 -> getArbitrary[Eval[Streaming[A]]].map(Streaming.wait(_)),
        // Cons
        6 -> (for {
          a <- getArbitrary[A]
          tail <- getArbitrary[Eval[Streaming[A]]]
        } yield Streaming.cons(a, tail)))
    }

  implicit def streamingArbitrary[A:Arbitrary]: Arbitrary[Streaming[A]] =
    Arbitrary(streamingGen[A](8))

  def emptyStreamingTGen[F[_], A]: Gen[StreamingT[F, A]] =
    Gen.const(StreamingT.empty[F, A])

  def streamingTGen[F[_], A](maxDepth: Int)(implicit F: Monad[F], A: Arbitrary[A]): Gen[StreamingT[F, A]] = {
    if (maxDepth <= 1)
      emptyStreamingTGen[F, A]
    else Gen.frequency(
      // Empty
      1 -> emptyStreamingTGen[F, A],
      // Wait
      2 -> streamingTGen[F, A](maxDepth - 1).map(s =>
        StreamingT.wait(F.pure(s))),
      // Cons
      6 -> (for {
        a <- A.arbitrary
        s <- streamingTGen[F, A](maxDepth - 1)
      } yield StreamingT.cons(a, F.pure(s))))
  }

  // The max possible size of a StreamingT instance (n) will result in
  // instances of up to n^3 in length when testing flatMap
  // composition. The current value (8) could result in streams of up
  // to 512 elements in length. Thus, since F may not be stack-safe,
  // we want to keep n relatively small.
  implicit def streamingTArbitrary[F[_], A](implicit F: Monad[F], A: Arbitrary[A]): Arbitrary[StreamingT[F, A]] =
    Arbitrary(streamingTGen[F, A](8))

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
