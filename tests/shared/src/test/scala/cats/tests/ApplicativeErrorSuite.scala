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
    // assertEquals(compileErrors("e3.attemptNarrow[List[Str.type]]"), "")
  }

  test("assert is equivalent to Predef.assert") {
    val A = ApplicativeError[Either[Throwable, *], Throwable]
    assertEquals(A.assert(true), A.unit)
    assertEquals(A.assert(true, "foo"), A.unit)
    val catsAssert = A.assert(false)
    val predefAssert = intercept[AssertionError] { Predef.assert(false) }
    assert(catsAssert.isLeft)
    assert(catsAssert.left.get.isInstanceOf[AssertionError])
    assertEquals(catsAssert.left.get.getMessage, predefAssert.getMessage)
    val catsAssertWithMsg = A.assert(false, "foo")
    val predefAssertWithMsg = intercept[AssertionError] { Predef.assert(false, "foo") }
    assert(catsAssertWithMsg.isLeft)
    assert(catsAssertWithMsg.left.get.isInstanceOf[AssertionError])
    assertEquals(catsAssertWithMsg.left.get.getMessage, predefAssertWithMsg.getMessage)
  }

  test("assume is equivalent to Predef.assume") {
    val A = ApplicativeError[Either[Throwable, *], Throwable]
    assertEquals(A.assume(true), A.unit)
    assertEquals(A.assume(true, "foo"), A.unit)
    val catsAssume = A.assume(false)
    val predefAssume = intercept[AssertionError] { Predef.assume(false) }
    assert(catsAssume.isLeft)
    assert(catsAssume.left.get.isInstanceOf[AssertionError])
    assertEquals(catsAssume.left.get.getMessage, predefAssume.getMessage)
    val catsAssumeWithMsg = A.assume(false, "foo")
    val predefAssumeWithMsg = intercept[AssertionError] { Predef.assume(false, "foo") }
    assert(catsAssumeWithMsg.isLeft)
    assert(catsAssumeWithMsg.left.get.isInstanceOf[AssertionError])
    assertEquals(catsAssumeWithMsg.left.get.getMessage, predefAssumeWithMsg.getMessage)
  }

  test("require is equivalent to Predef.require") {
    val A = ApplicativeError[Either[Throwable, *], Throwable]
    assertEquals(A.require(true), A.unit)
    assertEquals(A.require(true, "foo"), A.unit)
    val catsRequire = A.require(false)
    val predefRequire = intercept[IllegalArgumentException] { Predef.require(false) }
    assert(catsRequire.isLeft)
    assert(catsRequire.left.get.isInstanceOf[IllegalArgumentException])
    assertEquals(catsRequire.left.get.getMessage, predefRequire.getMessage)
    val catsRequireWithMsg = A.require(false, "foo")
    val predefRequireWithMsg = intercept[IllegalArgumentException] { Predef.require(false, "foo") }
    assert(catsRequireWithMsg.isLeft)
    assert(catsRequireWithMsg.left.get.isInstanceOf[IllegalArgumentException])
    assertEquals(catsRequireWithMsg.left.get.getMessage, predefRequireWithMsg.getMessage)
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
