package cats
package compat

private[cats] trait FoldableCompat[F[_]] { this: Foldable[F] =>

  def iterable[A](fa: F[A]): Stream[A] =
    foldRight[A, Stream[A]](fa, Eval.now(Stream.empty)) { (a, eb) =>
      eb.map(Stream.cons(a, _))
    }.value
}
