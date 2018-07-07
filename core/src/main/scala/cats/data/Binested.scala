package cats
package data

import cats.arrow._

/** Compose a two-slot type constructor `F[_, _]` with two single-slot type constructors
 *  `G[_]` and `H[_]`, resulting in a two-slot type constructor with respect to the inner types.
 *  For example, `List` and `Option` both have `Functor` instances, and `Either` has a
 *  `Bifunctor` instance. Therefore, `Binested[Either, List, Option, ?, ?]` has a `Bifunctor`
 *  instance as well:
 *
 * {{{
 * scala> import cats.Bifunctor
 * scala> import cats.data.Binested
 * scala> import cats.implicits._
 * scala> val eitherListOption: Either[List[Int], Option[String]] = Right(Some("cats"))
 * scala> val f: Int => String = _.toString
 * scala> val g: String => String = _ + "-bifunctor"
 * scala> val binested = Binested(eitherListOption)
 * scala> val bimapped = Bifunctor[Binested[Either, List, Option, ?, ?]].bimap(binested)(f, g).value
 * res0: Either[List[String], Option[String]] = Right(Some("cats-bifunctor"))
 * }}}
 */
final case class Binested[F[_, _], G[_], H[_], A, B](value: F[G[A], H[B]])

object Binested extends BinestedInstances

trait BinestedInstances extends BinestedInstances0 {
  implicit def catsDataEqForBinested[F[_, _], G[_], H[_], A, B](
    implicit F: Eq[F[G[A], H[B]]]): Eq[Binested[F, G, H, A, B]] =
    Eq.by(_.value)

  implicit def catsDataProfunctorForBinested[F[_, _], G[_], H[_]](
    implicit F: Profunctor[F], G: Functor[G], H: Functor[H]): Profunctor[Binested[F, G, H, ?, ?]] =
    new Profunctor[Binested[F, G, H, ?, ?]] {
      def dimap[A, B, C, D](fab: Binested[F, G, H, A, B])(f: C => A)(g: B => D): Binested[F, G, H, C, D] =
        Binested(F.dimap(fab.value)(G.map(_: G[C])(f))(H.map(_)(g)))
    }

  implicit def catsDataBitraverseForBinested[F[_, _], G[_], H[_]](
    implicit F0: Bitraverse[F], H0: Traverse[H], G0: Traverse[G]): Bitraverse[Binested[F, G, H, ?, ?]] =
    new BinestedBitraverse[F, G, H] {
      override implicit def F: Bitraverse[F] = F0
      override implicit def G: Traverse[G] = G0
      override implicit def H: Traverse[H] = H0
    }
}

trait BinestedInstances0 {
  implicit def catsDataBifoldableForBinested[F[_, _], G[_], H[_]](
    implicit F0: Bifoldable[F], G0: Foldable[G], H0: Foldable[H]): Bifoldable[Binested[F, G, H, ?, ?]] =
    new BinestedBifoldable[F, G, H] {
      override implicit def F: Bifoldable[F] = F0
      override implicit def G: Foldable[G] = G0
      override implicit def H: Foldable[H] = H0
    }

  implicit def catsDataBifunctorForBinested[F[_, _], G[_], H[_]](
    implicit F: Bifunctor[F], G: Functor[G], H: Functor[H]): Bifunctor[Binested[F, G, H, ?, ?]] =
    new Bifunctor[Binested[F, G, H, ?, ?]] {
      def bimap[A, B, C, D](fab: Binested[F, G, H, A, B])(f: A => C, g: B => D): Binested[F, G, H, C, D] =
        Binested(F.bimap(fab.value)(G.map(_)(f), H.map(_)(g)))
    }
}

sealed abstract class BinestedBifoldable[F[_, _], G[_], H[_]] extends Bifoldable[Binested[F, G, H, ?, ?]] {
  implicit def F: Bifoldable[F]
  implicit def G: Foldable[G]
  implicit def H: Foldable[H]

  def bifoldLeft[A, B, C](fab: Binested[F, G, H, A, B], c: C)(f: (C, A) => C, g: (C, B) => C): C =
    F.bifoldLeft(fab.value, c)(
      (c, ga) => G.foldLeft(ga, c)(f),
      (c, hb) => H.foldLeft(hb, c)(g)
    )


  def bifoldRight[A, B, C](fab: Binested[F, G, H, A, B], c: Eval[C])(f: (A, Eval[C]) => Eval[C], g: (B, Eval[C]) => Eval[C]): Eval[C] =
    F.bifoldRight(fab.value, c)(
      (ga, ec) => G.foldRight(ga, ec)(f),
      (hb, ec) => H.foldRight(hb, ec)(g)
    )
}

sealed abstract class BinestedBitraverse[F[_, _], G[_], H[_]] extends BinestedBifoldable[F, G, H] with Bitraverse[Binested[F, G, H, ?, ?]] {
  override implicit def F: Bitraverse[F]
  override implicit def G: Traverse[G]
  override implicit def H: Traverse[H]

  def bitraverse[I[_], A, B, C, D](fab: Binested[F, G, H, A, B])(f: A => I[C], g: B => I[D])(implicit I: Applicative[I]): I[Binested[F, G, H, C, D]] = {
    I.map(F.bitraverse(fab.value)(G.traverse(_)(f), H.traverse(_)(g)))(Binested(_))
  }
}
