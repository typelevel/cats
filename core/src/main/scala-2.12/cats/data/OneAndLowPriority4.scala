package cats
package data

import scala.annotation.tailrec
import scala.collection.mutable.Builder

abstract private[data] class OneAndLowPriority4 {
  @deprecated("For collections, use special NonEmpty types instead of OneAnd (see #3089 for details)", "2.6.2")
  implicit val catsDataComonadForNonEmptyStream: Comonad[OneAnd[Stream, *]] =
    new Comonad[OneAnd[Stream, *]] {
      def coflatMap[A, B](fa: OneAnd[Stream, A])(f: OneAnd[Stream, A] => B): OneAnd[Stream, B] = {
        @tailrec def consume(as: Stream[A], buf: Builder[B, Stream[B]]): Stream[B] =
          if (as.isEmpty) buf.result()
          else {
            val tail = as.tail
            consume(tail, buf += f(OneAnd(as.head, tail)))
          }
        OneAnd(f(fa), consume(fa.tail, Stream.newBuilder))
      }

      def extract[A](fa: OneAnd[Stream, A]): A =
        fa.head

      def map[A, B](fa: OneAnd[Stream, A])(f: A => B): OneAnd[Stream, B] =
        fa.map(f)(cats.instances.stream.catsStdInstancesForStream)
    }
}
