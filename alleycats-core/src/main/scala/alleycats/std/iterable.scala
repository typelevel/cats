package alleycats
package std

import cats.{Eval, Foldable, Monoid}
import export._

@reexports(IterableInstances)
object iterable extends LegacyIterableInstances

@exports
object IterableInstances {
  @export(Orphan)
  implicit val exportIterableFoldable: Foldable[Iterable] =
    new Foldable[Iterable] {
      override def foldLeft[A, B](fa: Iterable[A], b: B)(f: (B, A) => B): B = fa.foldLeft(b)(f)

      override def foldRight[A, B](fa: Iterable[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        Foldable.iterateRight(fa, lb)(f)

      override def foldMap[A, B](fa: Iterable[A])(f: A => B)(implicit B: Monoid[B]): B =
        B.combineAll(fa.iterator.map(f))
    }

}

// TODO: remove when cats.Foldable support export-hook
trait LegacyIterableInstances {
  implicit def legacyIterableFoldable(implicit e: ExportOrphan[Foldable[Iterable]]): Foldable[Iterable] = e.instance
}
