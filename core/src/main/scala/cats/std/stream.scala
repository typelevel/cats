package cats
package std

import scala.annotation.tailrec

trait StreamInstances {
  implicit val streamInstance =
    new Traverse[Stream] with MonadCombine[Stream] with CoFlatMap[Stream] {

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

      // FIXME: laziness
      def foldRight[A, B](fa: Stream[A], b: B)(f: (A, B) => B): B =
        fa match {
          case Stream.Empty => b
          case a #:: rest => f(a, foldRight(rest, b)(f))
        }

      def foldRight[A, B](fa: Stream[A], b: Lazy[B])(f: (A, Lazy[B]) => B): Lazy[B] = {
        // we use Lazy.byName(...) to avoid memoizing intermediate values.
        def loop(as: Stream[A], b: Lazy[B]): Lazy[B] = 
          as match {
            case Stream.Empty => b
            case a #:: rest => Lazy.byName(f(a, foldRight(rest, b)(f)))
          }
        // we memoize the first "step" with Lazy(...).
        Lazy(loop(fa, b).force)
      }

      def traverse[G[_]: Applicative, A, B](fa: Stream[A])(f: A => G[B]): G[Stream[B]] = {
        val G = Applicative[G]
        val gsb = G.pure(Stream.empty[B])
        foldRight(fa, gsb) { (a, gsb) =>
          G.map2(f(a), gsb)((b: B, sb: Stream[B]) => b #:: sb)
        }
      }
    }
}
