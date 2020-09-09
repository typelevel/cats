package cats.tests

import cats.ApplicativeError
import cats.data.EitherT
import cats.kernel.Eq
import cats.syntax.applicativeError._
import cats.syntax.either._
import cats.syntax.option._
import cats.syntax.eq._

class ApplicativeErrorSuite extends CatsSuite {
  val failed: Option[Int] =
    ().raiseError[Option, Int]

  test("raiseError syntax creates an Option with the correct value") {
    assert(failed === (None: Option[Int]))
  }

  test("handleError syntax transforms an error to a success") {
    assert(failed.handleError(_ => 7) === (Some(7)))
  }

  test("handleErrorWith transforms an error to a success") {
    assert(failed.handleErrorWith(_ => Some(7)) === (Some(7)))
  }

  test("attempt syntax creates a wrapped Either") {
    assert(failed.attempt === (Option(Left(()))))
  }

  test("attemptNarrow[EE] syntax creates an F[Either[EE, A]]") {
    trait Err extends Throwable
    case class ErrA() extends Err
    case class ErrB() extends Err

    implicit val eqForErr: Eq[Err] = Eq.fromUniversalEquals[Err]
    implicit val eqForErrA: Eq[ErrA] = Eq.fromUniversalEquals[ErrA]
    implicit val eqForErrB: Eq[ErrB] = Eq.fromUniversalEquals[ErrB]

    val failed: Either[Err, Int] = ErrA().raiseError[Either[Err, *], Int]

    assert(failed.attemptNarrow[ErrA] === (ErrA().asLeft[Int].asRight[Err]))
    assert(failed.attemptNarrow[ErrB] === (Either.left[Err, Either[ErrB, Int]](ErrA())))
  }

  test("attemptNarrow works for parametrized types") {
    trait T[A] extends Throwable
    case object Str extends T[String]
    case class Num(i: Int) extends T[Int]

    implicit def eqForT[A]: Eq[T[A]] = Eq.fromUniversalEquals[T[A]]
    implicit val eqForStr: Eq[Str.type] = Eq.fromUniversalEquals[Str.type]
    implicit val eqForNum: Eq[Num] = Eq.fromUniversalEquals[Num]

    val e: Either[T[Int], Unit] = Num(1).asLeft[Unit]
    assert(e.attemptNarrow[Num] === (e.asRight[T[Int]]))
    assert(compileErrors("e.attemptNarrow[Str.type]").nonEmpty)

    val e2: Either[T[String], Unit] = Str.asLeft[Unit]
    assert(e2.attemptNarrow[Str.type] === (e2.asRight[T[String]]))
    assert(compileErrors("e2.attemptNarrow[Num]").nonEmpty)

    val e3: Either[List[T[String]], Unit] = List(Str).asLeft[Unit]
    assert(compileErrors("e3.attemptNarrow[List[Str.type]]").nonEmpty)
  }

  test("attemptT syntax creates an EitherT") {
    assert(failed.attemptT === (EitherT[Option, Unit, Int](Option(Left(())))))
  }

  test("recover syntax transforms an error to a success") {
    assert(failed.recover { case _ => 7 } === (Some(7)))
  }

  test("recoverWith transforms an error to a success") {
    assert(failed.recoverWith { case _ => Some(7) } === (Some(7)))
  }

  {
    final case class OptionWrapper[A](option: Option[A])

    implicit def mayBeApplicativeError[E](implicit
      ev: ApplicativeError[Option, E]
    ): ApplicativeError[OptionWrapper, E] =
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
      assert(OptionWrapper(17.some).orElse(OptionWrapper(None)).option === (OptionWrapper(17.some).option))
    }

    test("orElse transforms an error to the alternative") {
      assert(
        ().raiseError[OptionWrapper, Int].orElse(OptionWrapper(17.some)).option === (OptionWrapper(17.some).option)
      )
    }
  }
}
