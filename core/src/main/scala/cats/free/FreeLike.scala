package cats
package free

trait FreeLike[F[_], M[_]] {
  def wrap[A](a: F[M[A]]): M[A]
}
