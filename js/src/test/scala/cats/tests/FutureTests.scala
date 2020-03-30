package cats.js.tests

import cats.Comonad
import cats.instances.FutureInstances
import cats.js.instances.Await
import cats.js.instances.future.futureComonad
import cats.kernel.Eq
import cats.kernel.laws.discipline.{MonoidTests => MonoidLawTests, SemigroupTests => SemigroupLawTests}
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import cats.syntax.either._
import cats.tests.{CatsSuite, ListWrapper}
import org.scalacheck.{Arbitrary, Cogen}
import org.scalacheck.Arbitrary.arbitrary
import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.concurrent.duration._

class FutureTests extends CatsSuite with FutureInstances {
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

  implicit def eqfa[A: Eq]: Eq[Future[A]] =
    new Eq[Future[A]] {
      def eqv(fx: Future[A], fy: Future[A]): Boolean = {
        val fz = futureEither(fx).zip(futureEither(fy))
        Await.result(fz.map { case (tx, ty) => tx === ty }, timeout)
      }
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
  checkAll("Future[Int]", ComonadTests[Future](comonad).comonad[Int, Int, Int])
  checkAll("Future", MonadTests[Future].monad[Int, Int, Int])

  {
    implicit val F = ListWrapper.semigroup[Int]
    checkAll("Future[ListWrapper[Int]]", SemigroupLawTests[Future[ListWrapper[Int]]].semigroup)
  }

  checkAll("Future[Int]", MonoidLawTests[Future[Int]].monoid)
}
