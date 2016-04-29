package cats
package std

import cats.syntax.show._

trait StreamInstances extends cats.kernel.std.StreamInstances {
  implicit val streamInstance: Traverse[Stream] with MonadCombine[Stream] with CoflatMap[Stream] =
    new Traverse[Stream] with MonadCombine[Stream] with CoflatMap[Stream] {

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

      def traverse[G[_], A, B](fa: Stream[A])(f: A => G[B])(implicit G: Applicative[G]): G[Stream[B]] = {
        def init: G[Stream[B]] = G.pure(Stream.empty[B])

        // We use foldRight to avoid possible stack overflows. Since
        // we don't want to return a Eval[_] instance, we call .value
        // at the end.
        //
        // (We don't worry about internal laziness because traverse
        // has to evaluate the entire stream anyway.)
        foldRight(fa, Later(init)) { (a, lgsb) =>
          lgsb.map(gsb => G.map2(f(a), gsb)(_ #:: _))
        }.value
      }

      override def exists[A](fa: Stream[A])(p: A => Boolean): Boolean =
        fa.exists(p)

      override def forall[A](fa: Stream[A])(p: A => Boolean): Boolean =
        fa.forall(p)

      override def isEmpty[A](fa: Stream[A]): Boolean = fa.isEmpty
    }

  implicit def streamShow[A: Show]: Show[Stream[A]] =
    new Show[Stream[A]] {
      def show(fa: Stream[A]): String = if(fa.isEmpty) "Stream()" else s"Stream(${fa.head.show}, ?)"
    }
}
