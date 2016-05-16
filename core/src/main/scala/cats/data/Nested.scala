package cats
package data

import cats.functor._

/** Similar to [[cats.data.Prod]], but for nested composition.
  *
  * For instance, since both `List` and `Option` have a `Functor`, then so does
  * `List[Option[_]]`. This is represented by this data type via the instantiation
  * `Nested[List, Option, ?]`.
  *
  * {{{
  * scala> import cats.Functor
  * scala> import cats.data.Nested
  * scala> import cats.implicits._
  * scala> val listOption: List[Option[Int]] = List(Some(1), None)
  * scala> val f: Int => String = i => (i * 2).toString
  * scala> Functor[List].map(listOption)(opt => opt.map(f))
  * res0: List[Option[String]] = List(Some(2), None)
  * scala> val nested: Nested[List, Option, Int] = Nested(listOption)
  * scala> val result: Nested[List, Option, String] = Functor[Nested[List, Option, ?]].map(nested)(f)
  * scala> result.value
  * res1: List[Option[String]] = List(Some(2), None)
  * }}}
  */
final case class Nested[F[_], G[_], A](value: F[G[A]])

object Nested extends NestedInstances

private[data] sealed abstract class NestedInstances extends NestedInstances1 {
  implicit def nestedEq[F[_], G[_], A](implicit FGA: Eq[F[G[A]]]): Eq[Nested[F, G, A]] =
    FGA.on(_.value)

  implicit def nestedTraverse[F[_]: Traverse, G[_]: Traverse]: Traverse[Nested[F, G, ?]] =
    new NestedTraverse[F, G] {
      val F = Traverse[F]
      val G = Traverse[G]
    }
}

private[data] sealed abstract class NestedInstances1 extends NestedInstances2 {
  implicit def nestedReducible[F[_]: Reducible, G[_]: Reducible]: Reducible[Nested[F, G, ?]] =
    new NestedReducible[F, G] {
      val F = Reducible[F]
      val G = Reducible[G]
    }

  implicit def nestedContravariant[F[_]: Contravariant, G[_]: Contravariant]: Functor[Nested[F, G, ?]] =
    new NestedContravariant[F, G] {
      val F = Contravariant[F]
      val G = Contravariant[G]
    }
}

private[data] sealed abstract class NestedInstances2 extends NestedInstances3 {
  implicit def nestedFoldable[F[_]: Foldable, G[_]: Foldable]: Foldable[Nested[F, G, ?]] =
    new NestedFoldable[F, G] {
      val F = Foldable[F]
      val G = Foldable[G]
    }

  implicit def nestedContravariantCovariant[F[_]: Contravariant, G[_]: Functor]: Contravariant[Nested[F, G, ?]] =
    new NestedContravariantCovariant[F, G] {
      val F = Contravariant[F]
      val G = Functor[G]
    }
}

private[data] sealed abstract class NestedInstances3 extends NestedInstances4 {
  implicit def nestedAlternative[F[_]: Alternative, G[_]: Applicative]: Alternative[Nested[F, G, ?]] =
    new CompositeAlternative[F, G] {
      val F = Alternative[F]
      val G = Applicative[G]
    }

  implicit def nestedCovariantContravariant[F[_]: Functor, G[_]: Contravariant]: Contravariant[Nested[F, G, ?]] =
    new NestedCovariantContravariant[F, G] {
      val F = Functor[F]
      val G = Contravariant[G]
    }
}

private[data] sealed abstract class NestedInstances4 extends NestedInstances5 {
  implicit def nestedApplicative[F[_]: Applicative, G[_]: Applicative]: Applicative[Nested[F, G, ?]] =
    new NestedApplicative[F, G] {
      val F = Applicative[F]
      val G = Applicative[G]
    }

  implicit def nestedMonoidK[F[_]: MonoidK, G[_]]: MonoidK[Nested[F, G, ?]] =
    new NestedMonoidK[F, G] {
      val F = MonoidK[F]
    }
}

private[data] sealed abstract class NestedInstances5 extends NestedInstances6 {
  implicit def nestedApply[F[_]: Apply, G[_]: Apply]: Apply[Nested[F, G, ?]] =
    new NestedApply[F, G] {
      val F = Apply[F]
      val G = Apply[G]
    }

  implicit def nestedSemigroupK[F[_]: SemigroupK, G[_]]: SemigroupK[Nested[F, G, ?]] =
    new NestedSemigroupK[F, G] {
      val F = SemigroupK[F]
    }
}

private[data] sealed abstract class NestedInstances6 extends NestedInstances7 {
  implicit def nestedFunctor[F[_]: Functor, G[_]: Functor]: Functor[Nested[F, G, ?]] =
    new NestedFunctor[F, G] {
      val F = Functor[F]
      val G = Functor[G]
    }
}

private[data] sealed abstract class NestedInstances7 extends NestedInstances8 {
  implicit def nestedInvariant[F[_]: Invariant, G[_]: Invariant]: Invariant[Nested[F, G, ?]] =
    new NestedInvariant[F, G] {
      val F = Invariant[F]
      val G = Invariant[G]
    }
}

private[data] sealed abstract class NestedInstances8 extends NestedInstances9 {
  implicit def nestedInvariantCovariant[F[_]: Invariant, G[_]: Functor]: Invariant[Nested[F, G, ?]] =
    new NestedInvariantCovariant[F, G] {
      val F = Invariant[F]
      val G = Functor[G]
    }
}

private[data] sealed abstract class NestedInstances9 {
  implicit def nestedInvariantContravariant[F[_]: Invariant, G[_]: Contravariant]: Invariant[Nested[F, G, ?]] =
    new NestedInvariantContravariant[F, G] {
      val F = Invariant[F]
      val G = Contravariant[G]
    }
}

private[data] trait NestedInvariant[F[_], G[_]] extends Invariant[Nested[F, G, ?]] {
  def F: Invariant[F]
  def G: Invariant[G]

  override def imap[A, B](fga: Nested[F, G, A])(f: A => B)(g: B => A): Nested[F, G, B] =
    Nested(F.imap(fga.value)(ga => G.imap(ga)(f)(g))(gb => G.imap(gb)(g)(f)))
}

private[data] trait NestedFunctor[F[_], G[_]] extends Functor[Nested[F, G, ?]] with NestedInvariant[F, G] {
  def F: Functor[F]
  def G: Functor[G]

  override def map[A, B](fga: Nested[F, G, A])(f: A => B): Nested[F, G, B] =
    Nested(F.map(fga.value)(ga => G.map(ga)(f)))
}

private[data] trait NestedApply[F[_], G[_]] extends Apply[Nested[F, G, ?]] with NestedFunctor[F, G] {
  def F: Apply[F]
  def G: Apply[G]

  override def ap[A, B](ff: Nested[F, G, A => B])(fa: Nested[F, G, A]): Nested[F, G, B] =
    Nested(F.ap(F.map(ff.value)(gab => G.ap(gab)(_)))(fa.value))

  override def product[A, B](fa: Nested[F, G, A], fb: Nested[F, G, B]): Nested[F, G, (A, B)] =
    Nested(F.map2(fa.value, fb.value)(G.product))
}

private[data] trait NestedApplicative[F[_], G[_]] extends Applicative[Nested[F, G, ?]] with NestedApply[F, G] {
  def F: Applicative[F]
  def G: Applicative[G]

  override def pure[A](x: A): Nested[F, G, A] = Nested(F.pure(G.pure(x)))
}

private[data] trait NestedSemigroupK[F[_], G[_]] extends SemigroupK[Nested[F, G, ?]] {
  def F: SemigroupK[F]

  override def combineK[A](x: Nested[F, G, A], y: Nested[F, G, A]): Nested[F, G, A] = Nested(F.combineK(x.value, y.value))
}

private[data] trait NestedMonoidK[F[_], G[_]] extends MonoidK[Nested[F, G, ?]] with NestedSemigroupK[F, G] {
  def F: MonoidK[F]

  override def empty[A]: Nested[F, G, A] = Nested(F.empty)
}

private[data] trait CompositeAlternative[F[_], G[_]] extends Alternative[Nested[F, G, ?]] with NestedApplicative[F, G] with NestedMonoidK[F, G] {
  def F: Alternative[F]
}

private[data] trait NestedFoldable[F[_], G[_]] extends Foldable[Nested[F, G, ?]] {
  def F: Foldable[F]
  def G: Foldable[G]

  override def foldLeft[A, B](fga: Nested[F, G, A], b: B)(f: (B, A) => B): B =
    F.foldLeft(fga.value, b)((b, a) => G.foldLeft(a, b)(f))

  override def foldRight[A, B](fga: Nested[F, G, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    F.foldRight(fga.value, lb)((ga, lb) => G.foldRight(ga, lb)(f))
}

private[data] trait NestedTraverse[F[_], G[_]] extends Traverse[Nested[F, G, ?]] with NestedFoldable[F, G] with NestedFunctor[F, G] {
  def F: Traverse[F]
  def G: Traverse[G]

  override def traverse[H[_]: Applicative, A, B](fga: Nested[F, G, A])(f: A => H[B]): H[Nested[F, G, B]] =
    Applicative[H].map(F.traverse(fga.value)(ga => G.traverse(ga)(f)))(Nested(_))
}

private[data] trait NestedReducible[F[_], G[_]] extends Reducible[Nested[F, G, ?]] with NestedFoldable[F, G] {
  def F: Reducible[F]
  def G: Reducible[G]

  override def reduceLeftTo[A, B](fga: Nested[F, G, A])(f: A => B)(g: (B, A) => B): B = {
    def toB(ga: G[A]): B = G.reduceLeftTo(ga)(f)(g)
    F.reduceLeftTo(fga.value)(toB) { (b, ga) =>
      G.foldLeft(ga, b)(g)
    }
  }

  override def reduceRightTo[A, B](fga: Nested[F, G, A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B] = {
    def toB(ga: G[A]): B = G.reduceRightTo(ga)(f)(g).value
    F.reduceRightTo(fga.value)(toB) { (ga, lb) =>
      G.foldRight(ga, lb)(g)
    }
  }
}

private[data] trait NestedContravariant[F[_], G[_]] extends Functor[Nested[F, G, ?]] {
  def F: Contravariant[F]
  def G: Contravariant[G]

  override def map[A, B](fga: Nested[F, G, A])(f: A => B): Nested[F, G, B] =
    Nested(F.contramap(fga.value)(gb => G.contramap(gb)(f)))
}

private[data] trait NestedContravariantCovariant[F[_], G[_]] extends Contravariant[Nested[F, G, ?]] {
  def F: Contravariant[F]
  def G: Functor[G]

  override def contramap[A, B](fga: Nested[F, G, A])(f: B => A): Nested[F, G, B] =
    Nested(F.contramap(fga.value)(gb => G.map(gb)(f)))
}

private[data] trait NestedCovariantContravariant[F[_], G[_]] extends Contravariant[Nested[F, G, ?]] {
  def F: Functor[F]
  def G: Contravariant[G]

  override def contramap[A, B](fga: Nested[F, G, A])(f: B => A): Nested[F, G, B] =
    Nested(F.map(fga.value)(ga => G.contramap(ga)(f)))
}

private[data] trait NestedInvariantCovariant[F[_], G[_]] extends Invariant[Nested[F, G, ?]] {
  def F: Invariant[F]
  def G: Functor[G]

  override def imap[A, B](fga: Nested[F, G, A])(f: A => B)(g: B => A): Nested[F, G, B] =
    Nested(F.imap(fga.value)(ga => G.map(ga)(f))(gb => G.map(gb)(g)))
}

private[data] trait NestedInvariantContravariant[F[_], G[_]] extends Invariant[Nested[F, G, ?]] {
  def F: Invariant[F]
  def G: Contravariant[G]

  override def imap[A, B](fga: Nested[F, G, A])(f: A => B)(g: B => A): Nested[F, G, B] =
    Nested(F.imap(fga.value)(ga => G.contramap(ga)(g))(gb => G.contramap(gb)(f)))
}
