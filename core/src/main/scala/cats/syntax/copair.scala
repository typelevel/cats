package cats
package syntax

trait CopairSyntax {
  implicit def copairSyntax[F[_, _]: Copair, A, B](fab: F[A, B]): CopairOps[F, A, B] =
    new CopairOps[F, A, B](fab)

  implicit def copairIdSyntax[A](a: A): CopairIdOps[A] = new CopairIdOps[A](a)

}

final class CopairOps[F[_,_], A, B](pair: F[A,B])(implicit F: Copair[F]) {

  def fold[C](fa: A => C, fb: B => C): C = F.fold(pair)(fa, fb)
  def swap: F[B,A] = F.swap(pair)

  def isRight: Boolean = F.isRight(pair)
  def isLeft:  Boolean = F.isLeft(pair)

  def foreach(fn: B => Unit): Unit = F.foreach(pair)(fn)

  def forall(fn: B => Boolean): Boolean = F.forall(pair)(fn)

  def exists(fn: B => Boolean): Boolean = F.exists(pair)(fn)

}

final class CopairIdOps[A](a: A) {
  def leftC[F[_,_], B](implicit F: Copair[F]): F[A,B] = F.left(a)
  def rightC[F[_,_], B](implicit F: Copair[F]): F[B,A] = F.right(a)
}