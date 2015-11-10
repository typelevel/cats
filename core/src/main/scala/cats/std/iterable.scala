package cats
package std

trait IterableInstances {
  implicit val iterableInstance: Foldable[Iterable] =
    new Foldable[Iterable] {

      def foldLeft[A, B](fa: Iterable[A], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b)(f)

      def foldRight[A, B](fa: Iterable[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
        val it = fa.iterator
        def loop(): Eval[B] =
          if (it.hasNext) f(it.next, Eval.defer(loop())) else lb
        Eval.defer(loop())
      }
    }
}
