package cats
package laws

trait FoldableLaws[F[_]] {
}

object FoldableLaws {
  def apply[F[_]](implicit ev: Foldable[F]): FoldableLaws[F] =
    new FoldableLaws[F] {}
}
