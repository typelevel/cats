package cats
package tests

import cats.data.EitherT

class ApplicativeErrorSuite extends CatsSuite {
  val failed: Option[Int] =
    (()).raiseError[Option, Int]

  test("raiseError syntax creates an Option with the correct value") {
    failed should === (None: Option[Int])
  }

  test("handleError syntax transforms an error to a success") {
    failed.handleError(_ => 7) should === (Some(7))
  }

  test("handleErrorWith transforms an error to a success") {
    failed.handleErrorWith(_ => Some(7)) should === (Some(7))
  }

  test("attempt syntax creates a wrapped Either") {
    failed.attempt should === (Option(Left(())))
  }

  test("attemptT syntax creates an EitherT") {
    failed.attemptT should === (EitherT[Option, Unit, Int](Option(Left(()))))
  }

  test("recover syntax transforms an error to a success") {
    failed.recover { case _ => 7 } should === (Some(7))
  }

  test("recoverWith transforms an error to a success") {
    failed.recoverWith { case _ => Some(7) } should === (Some(7))
  }

  {
    final case class OptionWrapper[A](option: Option[A])

    implicit def mayBeApplicativeError[E](implicit ev: ApplicativeError[Option, E]): ApplicativeError[OptionWrapper, E] =
      new ApplicativeError[OptionWrapper, E] {

        def raiseError[A](e: E): OptionWrapper[A] =
          OptionWrapper(ev.raiseError(e))

        def handleErrorWith[A](fa: OptionWrapper[A])(f: E => OptionWrapper[A]): OptionWrapper[A] =
          OptionWrapper(ev.handleErrorWith(fa.option)(f(_).option))

        def pure[A](x: A): OptionWrapper[A] =
          OptionWrapper(x.some)

        def ap[A, B](ff: OptionWrapper[A => B])(fa: OptionWrapper[A]): OptionWrapper[B] =
          OptionWrapper(ev.ap(ff.option)(fa.option))
      }

    test("orElse leaves a success unchanged") {
      (OptionWrapper(17.some) orElse OptionWrapper(None)).option should === (OptionWrapper(17.some).option)
    }

    test("orElse transforms an error to the alternative") {
      (().raiseError[OptionWrapper, Int] orElse OptionWrapper(17.some)).option should === (OptionWrapper(17.some).option)
    }
  }
}
