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

package cats.jvm.tests

import cats.kernel.{Eq, Semigroup}
import cats.kernel.laws.discipline.{MonoidTests => MonoidLawTests, SemigroupTests => SemigroupLawTests}
import cats.laws.discipline.*
import cats.laws.discipline.arbitrary.*
import cats.syntax.either.*
import cats.tests.{CatsSuite, ListWrapper}
import org.scalacheck.{Arbitrary, Cogen}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.rng.Seed
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global

class FutureSuite extends CatsSuite {
  val timeout = 3.seconds

  def futureEither[A](f: Future[A]): Future[Either[Throwable, A]] =
    f.map(Either.right[Throwable, A]).recover { case t => Either.left(t) }

  implicit def eqfa[A: Eq]: Eq[Future[A]] = { (fx, fy) =>
    val fz = futureEither(fx).zip(futureEither(fy))
    Await.result(fz.map { case (tx, ty) => tx === ty }, timeout)
  }

  implicit def cogen[A: Cogen]: Cogen[Future[A]] =
    Cogen[Future[A]] { (seed: Seed, t: Future[A]) =>
      Cogen[A].perturb(seed, Await.result(t, timeout))
    }

  implicit val throwableEq: Eq[Throwable] =
    Eq.by[Throwable, String](_.toString)

  // Need non-fatal Throwables for Future recoverWith/handleError
  implicit val nonFatalArbitrary: Arbitrary[Throwable] =
    Arbitrary(arbitrary[Exception].map(identity))

  checkAll("Future with Throwable", MonadErrorTests[Future, Throwable].monadError[Int, Int, Int])
  checkAll("Future", MonadTests[Future].monad[Int, Int, Int])
  checkAll("Future", CoflatMapTests[Future].coflatMap[Int, Int, Int])

  {
    implicit val F: Semigroup[ListWrapper[Int]] = ListWrapper.semigroup[Int]
    checkAll("Future[ListWrapper[Int]]", SemigroupLawTests[Future[ListWrapper[Int]]].semigroup)
  }

  checkAll("Future[Int]", MonoidLawTests[Future[Int]].monoid)
}
