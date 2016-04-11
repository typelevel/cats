package cats

trait Copair[F[_,_]] {
  def left[A,B](a: A): F[A,B]
  def right[A,B](b: B): F[A,B]

  def fold[A,B,C](f: F[A,B])(fa: A => C, fb: B => C): C

  def isRight[A,B](f: F[A,B]): Boolean = fold(f)(_ => false, _ => true )
  def isLeft[A,B](f: F[A,B]):  Boolean = fold(f)(_ => true , _ => false)

  def foreach[A,B](f: F[A,B])(fn: B => Unit): Unit = fold(f)(_ => (), fn)

  def swap[A,B](f: F[A,B]): F[B,A] = fold(f)(right[B, A], left[B, A])

  def forall[A, B](f: F[A,B])(fn: B => Boolean): Boolean = fold(f)(_ => true, fn)

  def exists[A, B](f: F[A,B])(fn: B => Boolean): Boolean = fold(f)(_ => false, fn)
}