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

package cats.js.tests

import cats.Comonad
import cats.js.instances.Await
import cats.js.instances.future.futureComonad
import cats.kernel.Eq
import cats.kernel.laws.discipline.{MonoidTests => MonoidLawTests, SemigroupTests => SemigroupLawTests}
import cats.laws.discipline.*
import cats.laws.discipline.arbitrary.*
import cats.syntax.either.*
import cats.tests.{CatsSuite, ListWrapper}
import org.scalacheck.{Arbitrary, Cogen}
import org.scalacheck.Arbitrary.arbitrary
import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.concurrent.duration.*

class FutureTests extends CatsSuite {
  // Replaces Scala.js's `JSExecutionContext.runNow`, which is removed in 1.0.
  // TODO: We shouldn't do this! See: https://github.com/scala-js/scala-js/issues/2102
  implicit private object RunNowExecutionContext extends ExecutionContextExecutor {
    def execute(runnable: Runnable): Unit =
      try {
        runnable.run()
      } catch {
        case t: Throwable => reportFailure(t)
      }

    def reportFailure(t: Throwable): Unit =
      t.printStackTrace()
  }

  val timeout = 3.seconds

  def futureEither[A](f: Future[A]): Future[Either[Throwable, A]] =
    f.map(Either.right[Throwable, A]).recover { case t => Either.left(t) }

  implicit def eqfa[A: Eq]: Eq[Future[A]] = { (fx, fy) =>
    val fz = futureEither(fx).zip(futureEither(fy))
    Await.result(fz.map { case (tx, ty) => tx === ty }, timeout)
  }

  implicit val throwableEq: Eq[Throwable] =
    Eq.by[Throwable, String](_.toString)

  val comonad: Comonad[Future] = futureComonad(timeout)

  // Need non-fatal Throwables for Future recoverWith/handleError
  implicit val nonFatalArbitrary: Arbitrary[Throwable] =
    Arbitrary(arbitrary[Exception].map(identity))

  // We can't block on futures in JS, so we can't create interesting
  // cogen instances. This will allow the tests to run in a
  // less-useful way.
  implicit def cogenForFuture[A]: Cogen[Future[A]] =
    Cogen[Unit].contramap(_ => ())

  checkAll("Future[Int]", MonadErrorTests[Future, Throwable].monadError[Int, Int, Int])
  checkAll("Future[Int]", ComonadTests[Future](using comonad).comonad[Int, Int, Int])
  checkAll("Future", MonadTests[Future].monad[Int, Int, Int])

  {
    implicit val F = ListWrapper.semigroup[Int]
    checkAll("Future[ListWrapper[Int]]", SemigroupLawTests[Future[ListWrapper[Int]]].semigroup)
  }

  checkAll("Future[Int]", MonoidLawTests[Future[Int]].monoid)
}
