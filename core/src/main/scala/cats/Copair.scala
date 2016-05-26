package cats

trait Copair[F[_,_]] extends Bitraverse[F] {
  def left[A,B](a: A): F[A,B]
  def right[A,B](b: B): F[A,B]

  def fold[A,B,C](f: F[A,B])(fa: A => C, fb: B => C): C

  def isRight[A,B](f: F[A,B]): Boolean = fold(f)(_ => false, _ => true )
  def isLeft[A,B](f: F[A,B]):  Boolean = fold(f)(_ => true , _ => false)

  def foreach[A,B](f: F[A,B])(fn: B => Unit): Unit = fold(f)(_ => (), fn)

  def swap[A,B](f: F[A,B]): F[B,A] = fold(f)(right[B, A], left[B, A])

  def forall[A, B](f: F[A,B])(fn: B => Boolean): Boolean = fold(f)(_ => true, fn)

  def exists[A, B](f: F[A,B])(fn: B => Boolean): Boolean = fold(f)(_ => false, fn)

  def to[G[_, _], A, B](f: F[A, B])(implicit G: Copair[G]): G[A,B] = fold(f)(G.left, G.right)

  def bitraverse[G[_], A, B, C, D](fab: F[A, B])(f: A => G[C], g: B => G[D])(implicit G: Applicative[G]): G[F[C, D]] =
    fold(fab)(
      l => G.map(f(l))(left),
      r => G.map(g(r))(right)
    )

  def bifoldLeft[A, B, C](fab: F[A, B], c: C)(f: (C, A) => C, g: (C, B) => C): C =
    fold(fab)(
      l => f(c,l),
      r => g(c,r)
    )

  def bifoldRight[A, B, C](fab: F[A, B], c: Eval[C])(f: (A, Eval[C]) => Eval[C], g: (B, Eval[C]) => Eval[C]): Eval[C] =
    fold(fab)(
      l => f(l,c),
      r => g(r,c)
    )
}