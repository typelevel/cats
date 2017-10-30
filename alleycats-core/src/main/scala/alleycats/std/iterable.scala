package alleycats
package std

import cats.{Eval, Foldable}
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
    }

}

// TODO: remove when cats.Foldable support export-hook
trait LegacyIterableInstances {
  implicit def legacyIterableFoldable(implicit e: ExportOrphan[Foldable[Iterable]]): Foldable[Iterable] = e.instance
}
