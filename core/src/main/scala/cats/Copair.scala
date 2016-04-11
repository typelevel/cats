package cats

/**
  * Created by jbarber on 4/10/16.
  */
trait Copair[F[_,_]] {
  def left[A,B](a: A): F[A,B]
  def right[A,B](b: B): F[A,B]

  def fold[A,B,C](f: F[A,B])(fa: A => C, fb: B => C): C

  def isRight[A,B](f: F[A,B]): Boolean = fold(f)(_ => false, _ => true )
  def isLeft[A,B](f: F[A,B]):  Boolean = fold(f)(_ => true , _ => false)

  def foreach[A,B](f: F[A,B])(fn: B => Unit): Unit = fold(f)(_ => (), fn)

  def orElse[A, B, BB >: B](f: F[A,B])(fallback: F[A,BB]): F[A,BB] = fold(f)(_ => fallback, right(_))

  def recover[A, B, BB >: B](f: F[A,B])(pf: PartialFunction[A, BB]): F[A,BB] = fold(f)(pf andThen right, right(_))

  def recoverWith[A, B, AA >: A, BB >: B](f: F[A,B])(pf: PartialFunction[A, F[AA,BB]]): F[AA,BB] = fold(f)(pf, right(_))

  def swap[A,B](f: F[A,B]): F[B,A] = fold(f)(right[B, A], left[B, A])

  def valueOr[A, B, BB >: B](f: F[A,B])(fn: A => BB): BB = fold(f)(fn, identity)

  def forall[A, B](f: F[A,B](fn: B => Boolean): Boolean = fold(f)(_ => true, fn)

  def exists[A, B](f: F[A,B](fn: B => Boolean): Boolean = fold(f)(_ => false, fn)

  def ensure[A, B, AA >: A](f: F[A,B])(onFailure: => AA)(fn: B => Boolean): F[AA,B] =
    fold(f)(_ => this, b => if (fn(b)) f else left(onFailure))
}