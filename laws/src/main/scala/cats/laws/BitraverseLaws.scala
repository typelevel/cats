package cats
package laws

import cats.data.Nested
import cats.syntax.BitraverseSyntaxBinCompat0

trait BitraverseLaws[F[_, _]] extends BifoldableLaws[F] with BifunctorLaws[F] with BitraverseSyntaxBinCompat0 {
  implicit override def F: Bitraverse[F]

  def bitraverseIdentity[A, B](fab: F[A, B]): IsEq[F[A, B]] =
    fab <-> F.bitraverse[Id, A, B, A, B](fab)(identity, identity)

  def bitraverseCompose[G[_], A, B, C, D, E, H](
    fab: F[A, B],
    f: A => G[C],
    g: B => G[D],
    h: C => G[E],
    i: D => G[H]
  )(implicit
    G: Applicative[G]): IsEq[G[G[F[E, H]]]] = {
    val fg = F.bitraverse(fab)(f, g)
    val hi = G.map(fg)(f => F.bitraverse(f)(h, i))

    val c =
      F.bitraverse[Nested[G, G, ?], A, B, E, H](fab)(
        a => Nested(G.map(f(a))(h)),
        b => Nested(G.map(g(b))(i))
      )

    hi <-> c.value
  }

  def leftTraverseIdentity[A, B](fab: F[A, B]): IsEq[F[A, B]] =
    fab <-> fab.leftTraverse[Id, A](identity)

  def leftTraverseCompose[G[_], A, B, C, D](
    fab: F[A, B],
    f: A => G[C],
    g: C => G[D]
  )(implicit G: Applicative[G]): IsEq[G[G[F[D, B]]]] = {
    val fg = G.map(fab.leftTraverse(f))(f => f.leftTraverse(g))
    val fg2 = fab.leftTraverse(a => Nested(G.map(f(a))(g)))

    fg <-> fg2.value
  }

}

object BitraverseLaws {
  def apply[F[_, _]](implicit ev: Bitraverse[F]): BitraverseLaws[F] =
    new BitraverseLaws[F] { def F: Bitraverse[F] = ev }
}
