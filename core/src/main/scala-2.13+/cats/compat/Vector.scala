package cats
package compat

private[cats] object Vector {
  def zipWith[A, B, C](fa: Vector[A], fb: Vector[B])(f: (A, B) => C): Vector[C] =
    fa.lazyZip(fb).map(f)
}
