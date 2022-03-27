package alleycats
package std

import cats.data.Chain
import cats.kernel.instances.StaticMethods.wrapMutableIndexedSeq
import cats.{Applicative, Eval, Foldable, Monoid, Traverse}

object iterable extends IterableInstances

trait IterableInstances {
  implicit def alleycatsStdIterableTraverse: Traverse[Iterable] = IterableInstances.alleycatsStdIterableTraverse

  @deprecated("use alleycatsStdIterableTraverse", "2.7.1")
  val alleycatsStdIterableFoldable: Foldable[Iterable] = alleycatsStdIterableTraverse
}

private[std] object IterableInstances {
  val alleycatsStdIterableTraverse: Traverse[Iterable] =
    new Traverse[Iterable] {
      override def foldLeft[A, B](fa: Iterable[A], b: B)(f: (B, A) => B): B = fa.foldLeft(b)(f)

      override def foldRight[A, B](fa: Iterable[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        Foldable.iterateRight(fa, lb)(f)

      override def foldMap[A, B](fa: Iterable[A])(f: A => B)(implicit B: Monoid[B]): B =
        B.combineAll(fa.iterator.map(f))

      override def reduceLeftOption[A](fa: Iterable[A])(f: (A, A) => A): Option[A] = fa.reduceLeftOption(f)

      override def collectFirst[A, B](fa: Iterable[A])(pf: PartialFunction[A, B]): Option[B] = fa.collectFirst(pf)

      override def fold[A](fa: Iterable[A])(implicit A: Monoid[A]): A = fa.fold(A.empty)(A.combine)

      override def find[A](fa: Iterable[A])(f: A => Boolean): Option[A] = fa.find(f)

      override def toIterable[A](fa: Iterable[A]): Iterable[A] = fa

      override def exists[A](fa: Iterable[A])(p: A => Boolean): Boolean = fa.exists(p)

      override def forall[A](fa: Iterable[A])(p: A => Boolean): Boolean = fa.forall(p)

      override def toList[A](fa: Iterable[A]): List[A] = fa.toList

      override def isEmpty[A](fa: Iterable[A]): Boolean = fa.isEmpty

      override def nonEmpty[A](fa: Iterable[A]): Boolean = fa.nonEmpty

      // copied from List instances
      override def traverse[G[_], A, B](fa: Iterable[A])(f: A => G[B])(implicit G: Applicative[G]): G[Iterable[B]] =
        if (fa.isEmpty) G.pure(Iterable.empty)
        else
          G.map(Chain.traverseViaChain {
            val as = collection.mutable.ArrayBuffer[A]()
            as ++= fa
            wrapMutableIndexedSeq(as)
          }(f))(_.toVector)
    }
}
