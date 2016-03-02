package cats
package data

import cats.functor.Bifunctor

/** Product composition for type classes abstracting over binary type constructors */
final case class BiProd[F[_, _], G[_, _], A, B](
  first: F[A, B],
  second: G[A, B]
)

object BiProd extends BiProdInstances

sealed trait BiProdInstances extends BiProdInstances0 {
  implicit def biprodBitraverse[F[_, _]: Bitraverse, G[_, _]: Bitraverse]:
      Bitraverse[BiProd[F, G, ?, ?]] =
    new BiProdBitraverse[F, G] {
      val F = Bitraverse[F]
      val G = Bitraverse[G]
    }

  implicit def biprodEq[F[_, _], G[_, _], A, B](implicit F: Eq[F[A, B]], G: Eq[G[A, B]]): Eq[BiProd[F, G, A, B]] =
    new Eq[BiProd[F, G, A, B]] {
      def eqv(x: BiProd[F, G, A, B], y: BiProd[F, G, A, B]): Boolean =
        F.eqv(x.first, y.first) && G.eqv(x.second, y.second)
    }
}

sealed trait BiProdInstances0 {
  implicit def biprodBifunctor[F[_, _]: Bifunctor, G[_, _]: Bifunctor]:
      Bifunctor[BiProd[F, G, ?, ?]] =
    new BiProdBifunctor[F, G] {
      val F = Bifunctor[F]
      val G = Bifunctor[G]
    }

  implicit def biprodBifoldable[F[_, _]: Bifoldable, G[_, _]: Bifoldable]:
      Bifoldable[BiProd[F, G, ?, ?]] =
    new BiProdBifoldable[F, G] {
      val F = Bifoldable[F]
      val G = Bifoldable[G]
    }
}

trait BiProdBifunctor[F[_, _], G[_, _]] extends Bifunctor[BiProd[F, G, ?, ?]] {
  def F: Bifunctor[F]
  def G: Bifunctor[G]

  def bimap[A, B, C, D](fab: BiProd[F, G, A, B])(f: A => C, g: B => D): BiProd[F, G, C, D] = {
    val left = F.bimap(fab.first)(f, g)
    val right = G.bimap(fab.second)(f, g)
    BiProd(left, right)
  }
}

trait BiProdBifoldable[F[_, _], G[_, _]] extends Bifoldable[BiProd[F, G, ?, ?]] {
  def F: Bifoldable[F]
  def G: Bifoldable[G]


  def bifoldLeft[A, B, C](fab: BiProd[F, G, A, B], c: C)(f: (C, A) => C, g: (C, B) => C): C = {
    val left = F.bifoldLeft(fab.first, c)(f, g)
    G.bifoldLeft(fab.second, left)(f, g)
  }

  def bifoldRight[A, B, C](fab: BiProd[F, G, A, B], c: Eval[C])(f: (A, Eval[C]) => Eval[C], g: (B, Eval[C]) => Eval[C]): Eval[C] = {
    val left = F.bifoldRight(fab.first, c)(f, g)
    G.bifoldRight(fab.second, left)(f, g)
  }
}

trait BiProdBitraverse[F[_, _], G[_, _]] extends Bitraverse[BiProd[F, G, ?, ?]] with BiProdBifoldable[F, G] with BiProdBifunctor[F, G] {
  def F: Bitraverse[F]
  def G: Bitraverse[G]

  def bitraverse[H[_]: Applicative, A, B, C, D](fab: BiProd[F, G, A, B])(f: A => H[C], g: B => H[D]): H[BiProd[F, G, C, D]] = {
    val left = F.bitraverse(fab.first)(f, g)
    val right = G.bitraverse(fab.second)(f, g)
    Applicative[H].map2(left, right) { case (l, r) => BiProd(l, r) }
  }

  override def bimap[A, B, C, D](fab: BiProd[F, G, A, B])(f: A => C, g: B => D): BiProd[F, G, C, D] =
    super[BiProdBifunctor].bimap(fab)(f, g)
}
