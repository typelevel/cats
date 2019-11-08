package cats
package compat

private[cats] object Foldable {

  def iterator[F[_], A](fa: F[A])(F: Foldable[F]): Iterator[A] =
    F.foldRight[A, Stream[A]](fa, Eval.now(Stream.empty)) { (a, eb) =>
        eb.map(Stream.cons(a, _))
      }
      .value
      .iterator
}
