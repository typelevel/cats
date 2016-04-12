package cats
package syntax


/**
  * Created by jbarber on 4/10/16.
  */
trait CopairSyntax {
  implicit def copairSyntax[F[_, _]: Copair, A, B](fab: F[A, B]): CopairOps[F, A, B] =
    new CopairOps[F, A, B](fab)

  implicit def copairIdSyntax[A](a: A): CopairIdOps[A] = new CopairIdOps[A](a)

  implicit def bitraverseFromCopair[F[_,_]](implicit F: Copair[F]): Bitraverse[F] =
    new Bitraverse[F] {
      def bitraverse[G[_], A, B, C, D](fab: F[A, B])(f: A => G[C], g: B => G[D])(implicit G: Applicative[G]): G[F[C, D]] =
        F.fold(fab)(
          l => G.map(f(l))(F.left),
          r => G.map(g(r))(F.right)
        )

      def bifoldLeft[A, B, C](fab: F[A, B], c: C)(f: (C, A) => C, g: (C, B) => C): C =
        F.fold(fab)(
          l => f(c,l),
          r => g(c,r)
        )

      def bifoldRight[A, B, C](fab: F[A, B], c: Eval[C])(f: (A, Eval[C]) => Eval[C], g: (B, Eval[C]) => Eval[C]): Eval[C] =
        F.fold(fab)(
          l => f(l,c),
          r => g(r,c)
        )
    }


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
  def left[F[_,_], B](implicit F: Copair[F]): F[A,B] = F.left(a)
  def right[F[_,_], B](implicit F: Copair[F]): F[B,A] = F.right(a)
}