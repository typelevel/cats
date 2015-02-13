package cats
package std

import scala.annotation.tailrec

trait StreamInstances {
  implicit val streamInstance: Traverse[Stream] with MonadCombine[Stream] with CoFlatMap[Stream] =
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

      // note: this foldRight variant is eager not lazy
      override def foldRight[A, B](fa: Stream[A], b: B)(f: (A, B) => B): B =
        fa.foldRight(b)(f)

      def partialFold[A, B](fa: Stream[A])(f: A => Fold[B]): Fold[B] =
        Fold.partialIterate(fa)(f)

      def traverse[G[_]: Applicative, A, B](fa: Stream[A])(f: A => G[B]): G[Stream[B]] = {
        val G = Applicative[G]
        // we use lazy to defer the creation of the stream's tail
        // until we are ready to prepend the stream's head with #::
        val gslb = G.pure(Lazy.byName(Stream.empty[B]))
        val gsb = foldRight(fa, gslb) { (a, lacc) =>
          G.map2(f(a), lacc)((b, acc) => Lazy.byName(b #:: acc.force))
        }
        // this only forces the first element of the stream, so we get
        // G[Stream[B]] instead of G[Lazy[Stream[B]]]. the rest of the
        // stream will be properly lazy.
        G.map(gsb)(_.force)
      }
    }
}
