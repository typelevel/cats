package cats
package syntax

/**
  * Created by jbarber on 4/10/16.
  */
trait CopairSyntax {
  implicit def copairSyntax[F[_, _]: Copair, A, B](fab: F[A, B]): CopairOps[F, A, B] =
    new CopairOps[F, A, B](fab)

  implicit def copairIdSyntax[A](a: A): CopairIdOps[A] = new CopairIdOps[A](a)

}

final class CopairOps[F[_,_]: Copair, A, B](pair: F[A,B]) {
  val ev = implicitly[Copair[F]]

  def fold[C](fa: A => C, fb: B => C): C = ev.fold(pair)(fa, fb)
  def swap: F[B,A] = ev.swap(pair)

  def isRight: Boolean = ev.isRight(pair)
  def isLeft:  Boolean = ev.isLeft(pair)

  def foreach(fn: B => Unit): Unit = ev.foreach(pair)(fn)

  def forall(fn: B => Boolean): Boolean = ev.forall(pair)(fn)

  def exists(fn: B => Boolean): Boolean = ev.forall(pair)(_ => false, fn)

}

final class CopairIdOps[A](a: A) {
  def left[F[_,_]: Copair, B]: F[A,B] = implicitly[Copair[F]].left(a)
  def right[F[_,_]: Copair, B]: F[B,A] = implicitly[Copair[F]].right(a)
}