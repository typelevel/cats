package cats
package js
package tests

import cats.data.FailFastFuture
import cats.kernel.laws.discipline.{MonoidTests => MonoidLawTests, SemigroupTests => SemigroupLawTests}
import cats.laws.discipline._
import cats.js.instances.Await
import cats.js.instances.future.futureComonad
import cats.tests.{CatsSuite, ListWrapper}

import scala.concurrent.Future
import scala.concurrent.duration._

import org.scalacheck.{Arbitrary, Cogen}
import org.scalacheck.Arbitrary.arbitrary
import cats.laws.discipline.arbitrary._

// https://issues.scala-lang.org/browse/SI-7934
@deprecated("", "")
class DeprecatedForwarder {
  implicit def runNow = scala.scalajs.concurrent.JSExecutionContext.Implicits.runNow
}
object DeprecatedForwarder extends DeprecatedForwarder
import DeprecatedForwarder.runNow

class FutureTests extends CatsSuite {
  val timeout = 3.seconds

  def futureEither[A](f: Future[A]): Future[Either[Throwable, A]] =
    f.map(Either.right[Throwable, A]).recover { case t => Either.left(t) }

  implicit def eqfa[A: Eq]: Eq[Future[A]] =
    new Eq[Future[A]] {
      def eqv(fx: Future[A], fy: Future[A]): Boolean = {
        val fz = futureEither(fx) zip futureEither(fy)
        Await.result(fz.map { case (tx, ty) => tx === ty }, timeout)
      }
    }

  implicit def eqffa[A: Eq]: Eq[FailFastFuture[A]] =
    Eq.by(_.value)


  implicit def failFastArbitrary[A: Arbitrary]: Arbitrary[FailFastFuture[A]] =
    Arbitrary(implicitly[Arbitrary[Future[A]]].arbitrary.map(FailFastFuture.apply))

  implicit val throwableEq: Eq[Throwable] =
    Eq.by[Throwable, String](_.toString)

  implicit val comonad: Comonad[Future] = futureComonad(timeout)

  // Need non-fatal Throwables for Future recoverWith/handleError
  implicit val nonFatalArbitrary: Arbitrary[Throwable] =
    Arbitrary(arbitrary[Exception].map(identity))

  // We can't block on futures in JS, so we can't create interesting
  // cogen instances. This will allow the tests to run in a
  // less-useful way.
  implicit def cogenForFuture[A]: Cogen[Future[A]] =
    Cogen[Unit].contramap(_ => ())

  checkAll("Future[Int]", MonadErrorTests[Future, Throwable].monadError[Int, Int, Int])
  checkAll("Future[Int]", ComonadTests[Future].comonad[Int, Int, Int])
  checkAll("Future", MonadTests[Future].monad[Int, Int, Int])
  checkAll("Parallel[Future, FailFastFuture]", ParallelTests[Future, FailFastFuture].parallel[Int, String])

  {
    implicit val F = ListWrapper.semigroup[Int]
    checkAll("Future[ListWrapper[Int]]", SemigroupLawTests[Future[ListWrapper[Int]]].semigroup)
  }

  checkAll("Future[Int]", MonoidLawTests[Future[Int]].monoid)
}
