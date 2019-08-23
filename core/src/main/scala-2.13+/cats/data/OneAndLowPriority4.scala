package cats
package data

import scala.annotation.tailrec
import scala.collection.mutable.Builder

abstract private[data] class OneAndLowPriority4 {
  @deprecated("Use catsDataComonadForNonEmptyLazyList", "2.0.0-RC2")
  implicit def catsDataComonadForNonEmptyStream: Comonad[OneAnd[Stream, *]] =
    new Comonad[OneAnd[Stream, *]] {
      def coflatMap[A, B](fa: OneAnd[Stream, A])(f: OneAnd[Stream, A] => B): OneAnd[Stream, B] = {
        @tailrec def consume(as: Stream[A], buf: Builder[B, Stream[B]]): Stream[B] =
          if (as.isEmpty) buf.result
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

  implicit val catsDataComonadForNonEmptyLazyList: Comonad[OneAnd[LazyList, *]] =
    new Comonad[OneAnd[LazyList, *]] {
      def coflatMap[A, B](fa: OneAnd[LazyList, A])(f: OneAnd[LazyList, A] => B): OneAnd[LazyList, B] = {
        @tailrec def consume(as: LazyList[A], buf: Builder[B, LazyList[B]]): LazyList[B] =
          if (as.isEmpty) buf.result
          else {
            val tail = as.tail
            consume(tail, buf += f(OneAnd(as.head, tail)))
          }
        OneAnd(f(fa), consume(fa.tail, LazyList.newBuilder))
      }

      def extract[A](fa: OneAnd[LazyList, A]): A =
        fa.head

      def map[A, B](fa: OneAnd[LazyList, A])(f: A => B): OneAnd[LazyList, B] =
        fa.map(f)(cats.instances.lazyList.catsStdInstancesForLazyList)
    }
}
