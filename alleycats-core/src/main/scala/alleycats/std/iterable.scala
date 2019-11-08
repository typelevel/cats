package alleycats
package std

import cats.{Eval, Foldable, Monoid}

object iterable extends IterableInstances

trait IterableInstances {
  implicit val alleycatsStdIterableFoldable: Foldable[Iterable] =
    new Foldable[Iterable] {
      override def foldLeft[A, B](fa: Iterable[A], b: B)(f: (B, A) => B): B = fa.foldLeft(b)(f)

      override def foldRight[A, B](fa: Iterable[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        Foldable.iterateRight(fa, lb)(f)

      override def foldMap[A, B](fa: Iterable[A])(f: A => B)(implicit B: Monoid[B]): B =
        B.combineAll(fa.iterator.map(f))

      override def iterator[A](fa: Iterable[A]): Iterator[A] = fa.iterator
    }
}
