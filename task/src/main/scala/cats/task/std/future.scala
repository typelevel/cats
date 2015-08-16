package cats
package task
package std

import java.util.concurrent.atomic.AtomicInteger

import scala.concurrent.{Await, CanAwait, ExecutionContext, Future, Promise}
import scala.concurrent.duration.Duration
import scala.util.{Try, Success}

import cats.data.StreamT
import cats.data.StreamT
import StreamT.{This, Next, Empty}

object future {
  def futureNondeterminism(implicit ec: ExecutionContext): Nondeterminism[Future] =
    new Nondeterminism[Future] {
      def pure[A](x: A): Future[A] = Future.successful(x)
      def flatMap[A, B](fa: Future[A])(f: A => Future[B]): Future[B] = fa.flatMap(f)
      override def map[A, B](fa: Future[A])(f: A => B): Future[B] = fa.map(f)

      def arrange[A](fas: List[Future[A]]): StreamT[Future, A] = {
        val limit = fas.size
        val counter = new AtomicInteger(0)
        val promises = new Array[Promise[A]](limit)
        fas.zipWithIndex.foreach { case (fa, i) =>
          promises(i) = Promise()
          fa.onComplete(t => promises(counter.getAndIncrement).tryComplete(t))
        }
        def evaluate(i: Int): Future[StreamT[Future, A]] =
          if (i == limit) pure(StreamT.empty)
          else promises(i).future.map { a =>
            StreamT.cons(a, evaluate(i + 1))
          }
        Next(evaluate(0))
      }
    }
}
