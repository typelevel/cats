package cats
package laws

import cats.data.Nested

trait Traverse2Laws[F[_, _]] extends Foldable2Laws[F] with Functor2Laws[F] {
  implicit override def F: Traverse2[F]

  def traverse2Identity[A, B](fab: F[A, B]): IsEq[F[A, B]] =
    fab <-> F.traverse2[Id, A, B, A, B](fab)(identity, identity)

  def traverse2Compose[G[_], A, B, C, D, E, H](
    fab: F[A, B],
    f: A => G[C],
    g: B => G[D],
    h: C => G[E],
    i: D => G[H]
  )(implicit
    G: Applicative[G]
  ): IsEq[G[G[F[E, H]]]] = {
    val fg = F.traverse2(fab)(f, g)
    val hi = G.map(fg)(f => F.traverse2(f)(h, i))

    val c =
      F.traverse2[Nested[G, G, ?], A, B, E, H](fab)(
        a => Nested(G.map(f(a))(h)),
        b => Nested(G.map(g(b))(i))
      )

    hi <-> c.value
  }
}

object Traverse2Laws {
  def apply[F[_, _]](implicit ev: Traverse2[F]): Traverse2Laws[F] =
    new Traverse2Laws[F] { def F: Traverse2[F] = ev }
}
