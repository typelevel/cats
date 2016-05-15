package cats
package data

import cats.functor._

final case class Compose[F[_], G[_], A](value: F[G[A]])

object Compose extends ComposeInstances

private[data] sealed abstract class ComposeInstances extends ComposeInstances1 {
  implicit def composeTraverse[F[_]: Traverse, G[_]: Traverse]: Traverse[Compose[F, G, ?]] =
    new ComposeTraverse[F, G] {
      val F = Traverse[F]
      val G = Traverse[G]
    }
}

private[data] sealed abstract class ComposeInstances1 extends ComposeInstances2 {
  /**
   * This composes two `Reducible` instances to provide an
   * instance for the nested types.
   *
   * In other words, given a `Reducible[F]` instance (which can reduce
   * `F[A]`) and a `Reducible[G]` instance (which can reduce `G[A]`
   * values), this class is able to reduce `F[G[A]]` values.
   */
  implicit def composeReducible[F[_]: Reducible, G[_]: Reducible]: Reducible[Compose[F, G, ?]] =
    new ComposeReducible[F, G] {
      val F = Reducible[F]
      val G = Reducible[G]
    }
}

private[data] sealed abstract class ComposeInstances2 extends ComposeInstances3 {
  implicit def composeFoldable[F[_]: Foldable, G[_]: Foldable]: Foldable[Compose[F, G, ?]] =
    new ComposeFoldable[F, G] {
      val F = Foldable[F]
      val G = Foldable[G]
    }
}

private[data] sealed abstract class ComposeInstances3 extends ComposeInstances4 {
  implicit def composeAlternative[F[_]: Alternative, G[_]: Applicative]: Alternative[Compose[F, G, ?]] =
    new CompositeAlternative[F, G] {
      val F = Alternative[F]
      val G = Applicative[G]
    }
}

private[data] sealed abstract class ComposeInstances4 extends ComposeInstances5 {
  /**
   * Two sequentially dependent Applicatives can be composed.
   *
   * The composition of Applicatives `F` and `G`, `F[G[x]]`, is also an Applicative
   *
   * Applicative[Option].compose[List].pure(10) = Some(List(10))
   */
  implicit def composeApplicative[F[_]: Applicative, G[_]: Applicative]: Applicative[Compose[F, G, ?]] =
    new ComposeApplicative[F, G] {
      val F = Applicative[F]
      val G = Applicative[G]
    }

  implicit def composeMonoidK[F[_]: MonoidK, G[_]]: MonoidK[Compose[F, G, ?]] =
    new ComposeMonoidK[F, G] {
      val F = MonoidK[F]
    }
}

private[data] sealed abstract class ComposeInstances5 extends ComposeInstances6 {
  /**
   * Two sequentially dependent Applys can be composed.
   *
   * The composition of Applys `F` and `G`, `F[G[x]]`, is also an Apply.
   *
   * Example:
   * {{{
   * scala> import cats.Apply
   * scala> import cats.implicits._
   * scala> val ap = Apply[Option].compose[List]
   * scala> val x: Option[List[Int]] = Some(List(1, 2))
   * scala> val y: Option[List[Int]] = Some(List(10, 20))
   * scala> ap.map2(x, y)(_ + _)
   * res0: Option[List[Int]] = Some(List(11, 21, 12, 22))
   * }}}
   */
  implicit def composeApply[F[_]: Apply, G[_]: Apply]: Apply[Compose[F, G, ?]] =
    new ComposeApply[F, G] {
      val F = Apply[F]
      val G = Apply[G]
    }

  implicit def composeSemigroupK[F[_]: SemigroupK, G[_]]: SemigroupK[Compose[F, G, ?]] =
    new ComposeSemigroupK[F, G] {
      val F = SemigroupK[F]
    }
}

private[data] sealed abstract class ComposeInstances6 extends ComposeInstances7 {
  implicit def composeFunctor[F[_]: Functor, G[_]: Functor]: Functor[Compose[F, G, ?]] =
    new ComposeFunctor[F, G] {
      val F = Functor[F]
      val G = Functor[G]
    }
}

private[data] sealed abstract class ComposeInstances7 {
  implicit def composeInvariant[F[_]: Invariant, G[_]: Invariant]: Invariant[Compose[F, G, ?]] =
    new ComposeInvariant[F, G] {
      val F = Invariant[F]
      val G = Invariant[G]
    }
}

private[data] trait ComposeInvariant[F[_], G[_]] extends Invariant[Compose[F, G, ?]] {
  def F: Invariant[F]
  def G: Invariant[G]

  override def imap[A, B](fga: Compose[F, G, A])(f: A => B)(g: B => A): Compose[F, G, B] =
    Compose(F.imap(fga.value)(ga => G.imap(ga)(f)(g))(gb => G.imap(gb)(g)(f)))
}

private[data] trait ComposeFunctor[F[_], G[_]] extends Functor[Compose[F, G, ?]] with ComposeInvariant[F, G] {
  def F: Functor[F]
  def G: Functor[G]

  override def map[A, B](fga: Compose[F, G, A])(f: A => B): Compose[F, G, B] =
    Compose(F.map(fga.value)(ga => G.map(ga)(f)))
}

private[data] trait ComposeApply[F[_], G[_]] extends Apply[Compose[F, G, ?]] with ComposeFunctor[F, G] {
  def F: Apply[F]
  def G: Apply[G]

  override def ap[A, B](ff: Compose[F, G, A => B])(fa: Compose[F, G, A]): Compose[F, G, B] =
    Compose(F.ap(F.map(ff.value)(gab => G.ap(gab)(_)))(fa.value))

  override def product[A, B](fa: F[G[A]], fb: F[G[B]]): F[G[(A, B)]] =
    F.map2(fa, fb)(G.product)
}

private[data] trait ComposeApplicative[F[_], G[_]] extends Applicative[Compose[F, G, ?]] with ComposeApply[F, G] {
  def F: Applicative[F]
  def G: Applicative[G]

  override def pure[A](x: A): Compose[F, G, A] = Compose(F.pure(G.pure(x)))
}

private[data] trait ComposeSemigroupK[F[_], G[_]] extends SemigroupK[Compose[F, G, ?]] {
  def F: SemigroupK[F]

  def combineK[A](x: Compose[F, G, A], y: Compose[F, G, A]): Compose[F, G, A] = Compose(F.combineK(x.value, y.value))
}

private[data] trait ComposeMonoidK[F[_], G[_]] extends MonoidK[Compose[F, G, ?]] with ComposeSemigroupK[F, G] {
  def F: MonoidK[F]

  def empty[A]: Compose[F, G, A] = Compose(F.empty)
}

private[data] trait CompositeAlternative[F[_], G[_]] extends Alternative[Compose[F, G, ?]] with ComposeApplicative[F, G] with ComposeMonoidK[F, G] {
  def F: Alternative[F]
}

private[data] trait ComposeFoldable[F[_], G[_]] extends Foldable[Compose[F, G, ?]] {
  def F: Foldable[F]
  def G: Foldable[G]

  def foldLeft[A, B](fga: Compose[F, G, A], b: B)(f: (B, A) => B): B =
    F.foldLeft(fga.value, b)((b, a) => G.foldLeft(a, b)(f))

  def foldRight[A, B](fga: Compose[F, G, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    F.foldRight(fga.value, lb)((ga, lb) => G.foldRight(ga, lb)(f))
}

private[data] trait ComposeTraverse[F[_], G[_]] extends Traverse[Compose[F, G, ?]] with ComposeFoldable[F, G] with ComposeFunctor[F, G] {
  def F: Traverse[F]
  def G: Traverse[G]

  def traverse[H[_]: Applicative, A, B](fga: Compose[F, G, A])(f: A => H[B]): H[Compose[F, G, B]] =
    Applicative[H].map(F.traverse(fga.value)(ga => G.traverse(ga)(f)))(Compose(_))
}

private[data] trait ComposeReducible[F[_], G[_]] extends Reducible[Compose[F, G, ?]] with ComposeFoldable[F, G] {
  def F: Reducible[F]
  def G: Reducible[G]

  override def reduceLeftTo[A, B](fga: Compose[F, G, A])(f: A => B)(g: (B, A) => B): B = {
    def toB(ga: G[A]): B = G.reduceLeftTo(ga)(f)(g)
    F.reduceLeftTo(fga.value)(toB) { (b, ga) =>
      G.foldLeft(ga, b)(g)
    }
  }

  override def reduceRightTo[A, B](fga: Compose[F, G, A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B] = {
    def toB(ga: G[A]): B = G.reduceRightTo(ga)(f)(g).value
    F.reduceRightTo(fga.value)(toB) { (ga, lb) =>
      G.foldRight(ga, lb)(g)
    }
  }
}
