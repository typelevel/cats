package cats.data

import cats.Applicative

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success}

class FailFastFuture[A](val value: Future[A]) extends AnyVal

object FailFastFuture {

  def apply[A](value: Future[A]): FailFastFuture[A] = new FailFastFuture(value)

  def catsDataApplicativeForFailFastFuture(implicit ec: ExecutionContext): Applicative[FailFastFuture] =
    new Applicative[FailFastFuture] {
      override def pure[A](x: A): FailFastFuture[A] = FailFastFuture(Future.successful(x))

      override def ap[A, B](ff: FailFastFuture[(A) => B])(fa: FailFastFuture[A]): FailFastFuture[B] = {
        val p = Promise[B]()

        ff.value.onComplete {
          case Failure(t) => p.tryFailure(t)
          case Success(_) => ()
        }

        fa.value.onComplete {
          case Failure(t) => p.tryFailure(t)
          case Success(_) => ()
        }

        p.tryCompleteWith(ff.value.flatMap(f => fa.value.map(f)))
        FailFastFuture(p.future)
      }
    }
}
