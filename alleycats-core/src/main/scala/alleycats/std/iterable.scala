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

      // based upon foldRight of List in Cats
      override def foldRight[A, B](fa: Iterable[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
        def loop(as: Iterable[A]): Eval[B] =
          if (as.isEmpty)
            lb
          else {
            val h = as.head
            val t = as.tail
            f(h, Eval.defer(loop(t)))
          }
        Eval.defer(loop(fa))
      }

    }

}

// TODO: remove when cats.Foldable support export-hook
trait LegacyIterableInstances {
  implicit def legacyIterableFoldable(implicit e: ExportOrphan[Foldable[Iterable]]): Foldable[Iterable] = e.instance

}
