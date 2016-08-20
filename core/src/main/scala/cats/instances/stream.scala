package cats
package instances

import cats.syntax.show._
import scala.annotation.tailrec

trait StreamInstances extends cats.kernel.instances.StreamInstances {
  implicit val catsStdInstancesForStream: TraverseFilter[Stream] with MonadCombine[Stream] with CoflatMap[Stream] with RecursiveTailRecM[Stream] =
    new TraverseFilter[Stream] with MonadCombine[Stream] with CoflatMap[Stream] with RecursiveTailRecM[Stream] {

      def empty[A]: Stream[A] = Stream.Empty

      def combineK[A](x: Stream[A], y: Stream[A]): Stream[A] = x #::: y

      def pure[A](x: A): Stream[A] = Stream(x)

      override def map[A, B](fa: Stream[A])(f: A => B): Stream[B] =
        fa.map(f)

      def flatMap[A, B](fa: Stream[A])(f: A => Stream[B]): Stream[B] =
        fa.flatMap(f)

      override def map2[A, B, Z](fa: Stream[A], fb: Stream[B])(f: (A, B) => Z): Stream[Z] =
        fa.flatMap(a => fb.map(b => f(a, b)))

      def coflatMap[A, B](fa: Stream[A])(f: Stream[A] => B): Stream[B] =
        fa.tails.toStream.init.map(f)

      def foldLeft[A, B](fa: Stream[A], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b)(f)

      def foldRight[A, B](fa: Stream[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        Now(fa).flatMap { s =>
          // Note that we don't use pattern matching to deconstruct the
          // stream, since that would needlessly force the tail.
          if (s.isEmpty) lb else f(s.head, Eval.defer(foldRight(s.tail, lb)(f)))
        }

      def traverseFilter[G[_], A, B](fa: Stream[A])(f: A => G[Option[B]])(implicit G: Applicative[G]): G[Stream[B]] = {
        // We use foldRight to avoid possible stack overflows. Since
        // we don't want to return a Eval[_] instance, we call .value
        // at the end.
        foldRight(fa, Always(G.pure(Stream.empty[B]))){ (a, lgsb) =>
          G.map2Eval(f(a), lgsb)((ob, s) => ob.fold(s)(_ #:: s))
        }.value
      }

      override def traverse[G[_], A, B](fa: Stream[A])(f: A => G[B])(implicit G: Applicative[G]): G[Stream[B]] = {
        // We use foldRight to avoid possible stack overflows. Since
        // we don't want to return a Eval[_] instance, we call .value
        // at the end.
        foldRight(fa, Always(G.pure(Stream.empty[B]))){ (a, lgsb) =>
          G.map2Eval(f(a), lgsb)(_ #:: _)
        }.value
      }

      def tailRecM[A, B](a: A)(fn: A => Stream[Either[A, B]]): Stream[B] = {
        val it: Iterator[B] = new Iterator[B] {
          var stack: Stream[Either[A, B]] = fn(a)
          var state: Either[Unit, Option[B]] = Left(())

          @tailrec
          def advance(): Unit = stack match {
            case Right(b) #:: tail =>
              stack = tail
              state = Right(Some(b))
            case Left(a) #:: tail =>
              stack = fn(a) #::: tail
              advance
            case empty =>
              state = Right(None)
          }

          @tailrec
          def hasNext: Boolean = state match {
            case Left(()) =>
              advance()
              hasNext
            case Right(o) =>
              o.isDefined
          }

          @tailrec
          def next(): B = state match {
            case Left(()) =>
              advance()
              next
            case Right(o) =>
              val b = o.get
              advance()
              b
          }
        }

        it.toStream
      }

      override def exists[A](fa: Stream[A])(p: A => Boolean): Boolean =
        fa.exists(p)

      override def forall[A](fa: Stream[A])(p: A => Boolean): Boolean =
        fa.forall(p)

      override def isEmpty[A](fa: Stream[A]): Boolean = fa.isEmpty

      override def filter[A](fa: Stream[A])(f: A => Boolean): Stream[A] = fa.filter(f)

      override def collect[A, B](fa: Stream[A])(f: PartialFunction[A, B]): Stream[B] = fa.collect(f)
    }

  implicit def catsStdShowForStream[A: Show]: Show[Stream[A]] =
    new Show[Stream[A]] {
      def show(fa: Stream[A]): String = if (fa.isEmpty) "Stream()" else s"Stream(${fa.head.show}, ?)"
    }
}
