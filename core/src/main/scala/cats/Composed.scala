/*
 * Copyright (c) 2015 Typelevel
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package cats

private[cats] trait ComposedDistributive[F[_], G[_]] extends Distributive[λ[α => F[G[α]]]] with ComposedFunctor[F, G] {
  outer =>
  def F: Distributive[F]
  def G: Distributive[G]

  override def distribute[H[_]: Functor, A, B](ha: H[A])(f: A => F[G[B]]): F[G[H[B]]] =
    F.map(F.distribute(ha)(f))(G.cosequence(_))
}

private[cats] trait ComposedInvariant[F[_], G[_]] extends Invariant[λ[α => F[G[α]]]] { outer =>
  def F: Invariant[F]
  def G: Invariant[G]

  override def imap[A, B](fga: F[G[A]])(f: A => B)(g: B => A): F[G[B]] =
    F.imap(fga)(ga => G.imap(ga)(f)(g))(gb => G.imap(gb)(g)(f))
}

private[cats] trait ComposedFunctor[F[_], G[_]] extends Functor[λ[α => F[G[α]]]] with ComposedInvariant[F, G] { outer =>
  def F: Functor[F]
  def G: Functor[G]

  override def map[A, B](fga: F[G[A]])(f: A => B): F[G[B]] =
    F.map(fga)(ga => G.map(ga)(f))
}

private[cats] trait ComposedFunctorBifunctor[F[_], G[_, _]] extends Bifunctor[λ[(α, β) => F[G[α, β]]]] { outer =>
  def F: Functor[F]
  def G: Bifunctor[G]

  def bimap[W, X, Y, Z](fab: F[G[W, X]])(f: W => Y, g: X => Z): F[G[Y, Z]] =
    F.map(fab)(G.bimap(_)(f, g))
}

private[cats] trait ComposedApply[F[_], G[_]] extends Apply[λ[α => F[G[α]]]] with ComposedFunctor[F, G] { outer =>
  def F: Apply[F]
  def G: Apply[G]

  override def ap[A, B](fgf: F[G[A => B]])(fga: F[G[A]]): F[G[B]] =
    F.ap(F.map(fgf)(gf => (ga: G[A]) => G.ap(gf)(ga)))(fga)

  override def product[A, B](fga: F[G[A]], fgb: F[G[B]]): F[G[(A, B)]] =
    F.map2(fga, fgb)(G.product)
}

private[cats] trait ComposedApplicative[F[_], G[_]] extends Applicative[λ[α => F[G[α]]]] with ComposedApply[F, G] {
  outer =>
  def F: Applicative[F]
  def G: Applicative[G]

  override def pure[A](x: A): F[G[A]] = F.pure(G.pure(x))
}

private[cats] trait ComposedSemigroupK[F[_], G[_]] extends SemigroupK[λ[α => F[G[α]]]] { outer =>
  def F: SemigroupK[F]

  override def combineK[A](x: F[G[A]], y: F[G[A]]): F[G[A]] = F.combineK(x, y)
}

private[cats] trait ComposedMonoidK[F[_], G[_]] extends MonoidK[λ[α => F[G[α]]]] with ComposedSemigroupK[F, G] {
  outer =>
  def F: MonoidK[F]

  override def empty[A]: F[G[A]] = F.empty
}

private[cats] trait ComposedNonEmptyAlternative[F[_], G[_]]
    extends NonEmptyAlternative[λ[α => F[G[α]]]]
    with ComposedApplicative[F, G]
    with ComposedSemigroupK[F, G] { outer =>

  def F: NonEmptyAlternative[F]
}

private[cats] trait ComposedAlternative[F[_], G[_]]
    extends Alternative[λ[α => F[G[α]]]]
    with ComposedNonEmptyAlternative[F, G]
    with ComposedMonoidK[F, G] { outer =>

  def F: Alternative[F]

  override def prependK[A](a: A, fa: F[G[A]]): F[G[A]] = F.prependK(G.pure(a), fa)

  override def appendK[A](fa: F[G[A]], a: A): F[G[A]] = F.appendK(fa, G.pure(a))
}

private[cats] trait ComposedFoldable[F[_], G[_]] extends Foldable[λ[α => F[G[α]]]] { outer =>
  def F: Foldable[F]
  def G: Foldable[G]

  override def foldLeft[A, B](fga: F[G[A]], b: B)(f: (B, A) => B): B =
    F.foldLeft(fga, b)((b, ga) => G.foldLeft(ga, b)(f))

  override def foldRight[A, B](fga: F[G[A]], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    F.foldRight(fga, lb)((ga, lb) => G.foldRight(ga, lb)(f))

  override def toList[A](fga: F[G[A]]): List[A] =
    F.toList(fga).flatMap(G.toList)

  override def toIterable[A](fga: F[G[A]]): Iterable[A] =
    F.toIterable(fga).flatMap(G.toIterable)
}

private[cats] trait ComposedTraverse[F[_], G[_]]
    extends Traverse[λ[α => F[G[α]]]]
    with ComposedFoldable[F, G]
    with ComposedFunctor[F, G] {
  def F: Traverse[F]
  def G: Traverse[G]

  override def traverse[H[_]: Applicative, A, B](fga: F[G[A]])(f: A => H[B]): H[F[G[B]]] =
    F.traverse(fga)(ga => G.traverse(ga)(f))

  override def mapAccumulate[S, A, B](init: S, fga: F[G[A]])(f: (S, A) => (S, B)): (S, F[G[B]]) =
    F.mapAccumulate(init, fga)((s, ga) => G.mapAccumulate(s, ga)(f))
}

private[cats] trait ComposedNonEmptyTraverse[F[_], G[_]]
    extends NonEmptyTraverse[λ[α => F[G[α]]]]
    with ComposedTraverse[F, G]
    with ComposedReducible[F, G] {
  def F: NonEmptyTraverse[F]
  def G: NonEmptyTraverse[G]

  override def nonEmptyTraverse[H[_]: Apply, A, B](fga: F[G[A]])(f: A => H[B]): H[F[G[B]]] =
    F.nonEmptyTraverse(fga)(ga => G.nonEmptyTraverse(ga)(f))
}

private[cats] trait ComposedReducible[F[_], G[_]] extends Reducible[λ[α => F[G[α]]]] with ComposedFoldable[F, G] {
  outer =>
  def F: Reducible[F]
  def G: Reducible[G]

  override def reduceLeftTo[A, B](fga: F[G[A]])(f: A => B)(g: (B, A) => B): B = {
    def toB(ga: G[A]): B = G.reduceLeftTo(ga)(f)(g)
    F.reduceLeftTo(fga)(toB) { (b, ga) =>
      G.foldLeft(ga, b)(g)
    }
  }

  override def reduceRightTo[A, B](fga: F[G[A]])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B] = {
    def toB(ga: G[A]): B = G.reduceRightTo(ga)(f)(g).value
    F.reduceRightTo(fga)(toB) { (ga, lb) =>
      G.foldRight(ga, lb)(g)
    }
  }
}

private[cats] trait ComposedContravariant[F[_], G[_]] extends Functor[λ[α => F[G[α]]]] { outer =>
  def F: Contravariant[F]
  def G: Contravariant[G]

  override def map[A, B](fga: F[G[A]])(f: A => B): F[G[B]] =
    F.contramap(fga)(gb => G.contramap(gb)(f))
}

private[cats] trait ComposedContravariantCovariant[F[_], G[_]] extends Contravariant[λ[α => F[G[α]]]] { outer =>
  def F: Contravariant[F]
  def G: Functor[G]

  override def contramap[A, B](fga: F[G[A]])(f: B => A): F[G[B]] =
    F.contramap(fga)(gb => G.map(gb)(f))
}

private[cats] trait ComposedApplicativeContravariantMonoidal[F[_], G[_]]
    extends ContravariantMonoidal[λ[α => F[G[α]]]] { outer =>
  def F: Applicative[F]
  def G: ContravariantMonoidal[G]

  override def unit: F[G[Unit]] = F.pure(G.unit)

  override def contramap[A, B](fa: F[G[A]])(f: B => A): F[G[B]] =
    F.map(fa)(G.contramap(_)(f))

  override def product[A, B](fa: F[G[A]], fb: F[G[B]]): F[G[(A, B)]] =
    F.map2(fa, fb)(G.product(_, _))
}

private[cats] trait ComposedSemigroupal[F[_], G[_]]
    extends ContravariantSemigroupal[λ[α => F[G[α]]]]
    with ComposedContravariantCovariant[F, G] { outer =>
  def F: ContravariantSemigroupal[F]
  def G: Functor[G]

  def product[A, B](fa: F[G[A]], fb: F[G[B]]): F[G[(A, B)]] =
    F.contramap(F.product(fa, fb)) { (g: G[(A, B)]) =>
      (G.map(g)(_._1), G.map(g)(_._2))
    }
}

private[cats] trait ComposedInvariantApplySemigroupal[F[_], G[_]]
    extends InvariantSemigroupal[λ[α => F[G[α]]]]
    with ComposedInvariantCovariant[F, G] { outer =>
  def F: InvariantSemigroupal[F]
  def G: Apply[G]

  def product[A, B](fa: F[G[A]], fb: F[G[B]]): F[G[(A, B)]] =
    F.imap(F.product(fa, fb)) { case (ga, gb) =>
      G.map2(ga, gb)(_ -> _)
    } { (g: G[(A, B)]) =>
      (G.map(g)(_._1), G.map(g)(_._2))
    }
}

private[cats] trait ComposedCovariantContravariant[F[_], G[_]] extends Contravariant[λ[α => F[G[α]]]] { outer =>
  def F: Functor[F]
  def G: Contravariant[G]

  override def contramap[A, B](fga: F[G[A]])(f: B => A): F[G[B]] =
    F.map(fga)(ga => G.contramap(ga)(f))
}

private[cats] trait ComposedInvariantCovariant[F[_], G[_]] extends Invariant[λ[α => F[G[α]]]] { outer =>
  def F: Invariant[F]
  def G: Functor[G]

  override def imap[A, B](fga: F[G[A]])(f: A => B)(g: B => A): F[G[B]] =
    F.imap(fga)(ga => G.map(ga)(f))(gb => G.map(gb)(g))
}

private[cats] trait ComposedInvariantContravariant[F[_], G[_]] extends Invariant[λ[α => F[G[α]]]] { outer =>
  def F: Invariant[F]
  def G: Contravariant[G]

  override def imap[A, B](fga: F[G[A]])(f: A => B)(g: B => A): F[G[B]] =
    F.imap(fga)(ga => G.contramap(ga)(g))(gb => G.contramap(gb)(f))
}
