package cats
package std

import scala.collection.immutable.Stream.Empty

trait StreamInstances {
  implicit val streamInstance: Traverse[Stream] with MonadCombine[Stream] with CoflatMap[Stream] =
    new Traverse[Stream] with MonadCombine[Stream] with CoflatMap[Stream] {

      def empty[A]: Stream[A] = Stream.Empty

      def combine[A](x: Stream[A], y: Stream[A]): Stream[A] = x #::: y

      def pure[A](x: A): Stream[A] = x #:: Stream.Empty

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

      def partialFold[A, B](fa: Stream[A])(f: A => Fold[B]): Fold[B] =
        Fold.partialIterate(fa)(f)

      def traverse[G[_]: Applicative, A, B](fa: Stream[A])(f: A => G[B]): G[Stream[B]] = {
        val G = Applicative[G]
        val init = G.pure(Stream.empty[B])

        // We use foldRight to avoid possible stack overflows. Since
        // we don't want to return a Lazy[_] instance, we call .value
        // at the end.
        //
        // (We don't worry about internal laziness because traverse
        // has to evaluate the entire stream anyway.)
        foldRight(fa, Lazy(init)) { a =>
          Fold.Continue(gsb => G.map2(f(a), gsb)(_ #:: _))
        }.value
      }
    }

  // TODO: eventually use algebra's instances (which will deal with
  // implicit priority between Eq/PartialOrder/Order).

  implicit def eqStream[A](implicit ev: Eq[A]): Eq[Stream[A]] =
    new Eq[Stream[A]] {
      def eqv(x: Stream[A], y: Stream[A]): Boolean = {
        def loop(xs: Stream[A], ys: Stream[A]): Boolean =
          xs match {
            case Empty => ys.isEmpty
            case a #:: xs =>
              ys match {
                case Empty => false
                case b #:: ys => if (ev.neqv(a, b)) false else loop(xs, ys)
              }
          }
        loop(x, y)
      }
    }

}
