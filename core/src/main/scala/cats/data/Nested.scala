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

private[data] sealed abstract class NestedInstances extends NestedInstances0 {
  implicit def catsDataEqForNested[F[_], G[_], A](implicit FGA: Eq[F[G[A]]]): Eq[Nested[F, G, A]] =
    FGA.on(_.value)

  implicit def catsDataTraverseFilterForNested[F[_]: Traverse, G[_]: TraverseFilter]: TraverseFilter[Nested[F, G, ?]] =
    new NestedTraverseFilter[F, G] {
      val FG: TraverseFilter[λ[α => F[G[α]]]] = Traverse[F].composeFilter[G]
    }
}

private[data] sealed abstract class NestedInstances0 extends NestedInstances1 {

  implicit def catsDataTraverseForNested[F[_]: Traverse, G[_]: Traverse]: Traverse[Nested[F, G, ?]] =
    new NestedTraverse[F, G] {
      val FG: Traverse[λ[α => F[G[α]]]] = Traverse[F].compose[G]
    }
}

private[data] sealed abstract class NestedInstances1 extends NestedInstances2 {
  implicit def catsDataReducibleForNested[F[_]: Reducible, G[_]: Reducible]: Reducible[Nested[F, G, ?]] =
    new NestedReducible[F, G] {
      val FG: Reducible[λ[α => F[G[α]]]] = Reducible[F].compose[G]
    }

  implicit def catsDataContravariantForNested[F[_]: Contravariant, G[_]: Contravariant]: Functor[Nested[F, G, ?]] =
    new NestedFunctor[F, G] {
      val FG: Functor[λ[α => F[G[α]]]] = Contravariant[F].compose[G]
    }
}

private[data] sealed abstract class NestedInstances2 extends NestedInstances3 {
  implicit def catsDataFoldableForNested[F[_]: Foldable, G[_]: Foldable]: Foldable[Nested[F, G, ?]] =
    new NestedFoldable[F, G] {
      val FG: Foldable[λ[α => F[G[α]]]] = Foldable[F].compose[G]
    }

  implicit def catsDataContravariantForCovariantNested[F[_]: Contravariant, G[_]: Functor]: Contravariant[Nested[F, G, ?]] =
    new NestedContravariant[F, G] {
      val FG: Contravariant[λ[α => F[G[α]]]] = Contravariant[F].composeFunctor[G]
    }
}

private[data] sealed abstract class NestedInstances3 extends NestedInstances4 {
  implicit def catsDataAlternativeForNested[F[_]: Alternative, G[_]: Applicative]: Alternative[Nested[F, G, ?]] =
    new NestedAlternative[F, G] {
      val FG: Alternative[λ[α => F[G[α]]]] = Alternative[F].compose[G]
    }

  implicit def catsDataContravariantForContravariantNested[F[_]: Functor, G[_]: Contravariant]: Contravariant[Nested[F, G, ?]] =
    new NestedContravariant[F, G] {
      val FG: Contravariant[λ[α => F[G[α]]]] = Functor[F].composeContravariant[G]
    }
}

private[data] sealed abstract class NestedInstances4 extends NestedInstances5 {
  implicit def catsDataApplicativeForNested[F[_]: Applicative, G[_]: Applicative]: Applicative[Nested[F, G, ?]] =
    new NestedApplicative[F, G] {
      val FG: Applicative[λ[α => F[G[α]]]] = Applicative[F].compose[G]
    }

  implicit def catsDataMonoidKForNested[F[_]: MonoidK, G[_]]: MonoidK[Nested[F, G, ?]] =
    new NestedMonoidK[F, G] {
      val FG: MonoidK[λ[α => F[G[α]]]] = MonoidK[F].compose[G]
    }
}

private[data] sealed abstract class NestedInstances5 extends NestedInstances6 {
  implicit def catsDataApplyForNested[F[_]: Apply, G[_]: Apply]: Apply[Nested[F, G, ?]] =
    new NestedApply[F, G] {
      val FG: Apply[λ[α => F[G[α]]]] = Apply[F].compose[G]
    }

  implicit def catsDataSemigroupKForNested[F[_]: SemigroupK, G[_]]: SemigroupK[Nested[F, G, ?]] =
    new NestedSemigroupK[F, G] {
      val FG: SemigroupK[λ[α => F[G[α]]]] = SemigroupK[F].compose[G]
    }
}

private[data] sealed abstract class NestedInstances6 extends NestedInstances7 {
  implicit def catsDataFunctorForNested[F[_]: Functor, G[_]: Functor]: Functor[Nested[F, G, ?]] =
    new NestedFunctor[F, G] {
      val FG: Functor[λ[α => F[G[α]]]] = Functor[F].compose[G]
    }
}

private[data] sealed abstract class NestedInstances7 extends NestedInstances8 {
  implicit def catsDataInvariantForNested[F[_]: Invariant, G[_]: Invariant]: Invariant[Nested[F, G, ?]] =
    new NestedInvariant[F, G] {
      val FG: Invariant[λ[α => F[G[α]]]] = Invariant[F].compose[G]
    }
}

private[data] sealed abstract class NestedInstances8 extends NestedInstances9 {
  implicit def catsDataInvariantForCovariantNested[F[_]: Invariant, G[_]: Functor]: Invariant[Nested[F, G, ?]] =
    new NestedInvariant[F, G] {
      val FG: Invariant[λ[α => F[G[α]]]] = Invariant[F].composeFunctor[G]
    }
}

private[data] sealed abstract class NestedInstances9 extends NestedInstances10 {
  implicit def catsDataFunctorFilterForNested[F[_]: Functor, G[_]: FunctorFilter]: FunctorFilter[Nested[F, G, ?]] =
    new NestedFunctorFilter[F, G] {
      val FG: FunctorFilter[λ[α => F[G[α]]]] = Functor[F].composeFilter[G]
    }
}

private[data] sealed abstract class NestedInstances10 {
  implicit def catsDataInvariantForNestedContravariant[F[_]: Invariant, G[_]: Contravariant]: Invariant[Nested[F, G, ?]] =
    new NestedInvariant[F, G] {
      val FG: Invariant[λ[α => F[G[α]]]] = Invariant[F].composeContravariant[G]
    }
}

private[data] trait NestedInvariant[F[_], G[_]] extends Invariant[Nested[F, G, ?]] {
  def FG: Invariant[λ[α => F[G[α]]]]

  override def imap[A, B](fga: Nested[F, G, A])(f: A => B)(g: B => A): Nested[F, G, B] =
    Nested(FG.imap(fga.value)(f)(g))
}

private[data] trait NestedFunctor[F[_], G[_]] extends Functor[Nested[F, G, ?]] with NestedInvariant[F, G] {
  override def FG: Functor[λ[α => F[G[α]]]]

  override def map[A, B](fga: Nested[F, G, A])(f: A => B): Nested[F, G, B] =
    Nested(FG.map(fga.value)(f))
}

private[data] trait NestedFunctorFilter[F[_], G[_]] extends FunctorFilter[Nested[F, G, ?]] with NestedFunctor[F, G] {
  override def FG: FunctorFilter[λ[α => F[G[α]]]]

  override def mapFilter[A, B](fga: Nested[F, G, A])(f: A => Option[B]): Nested[F, G, B] =
    Nested(FG.mapFilter(fga.value)(f))

  override def collect[A, B](fga: Nested[F, G, A])(f: PartialFunction[A, B]): Nested[F, G, B] =
    Nested(FG.collect(fga.value)(f))

  override def filter[A](fga: Nested[F, G, A])(f: A => Boolean): Nested[F, G, A] =
    Nested(FG.filter(fga.value)(f))
}

private[data] trait NestedTraverseFilter[F[_], G[_]] extends TraverseFilter[Nested[F, G, ?]] with NestedFunctorFilter[F, G] with NestedTraverse[F, G] {
  override def FG: TraverseFilter[λ[α => F[G[α]]]]

  override def traverseFilter[H[_]: Applicative, A, B](fga: Nested[F, G, A])(f: A => H[Option[B]]): H[Nested[F, G, B]] =
    Applicative[H].map(FG.traverseFilter(fga.value)(f))(Nested(_))
}

private[data] trait NestedApply[F[_], G[_]] extends Apply[Nested[F, G, ?]] with NestedFunctor[F, G] {
  override def FG: Apply[λ[α => F[G[α]]]]

  override def ap[A, B](fgf: Nested[F, G, A => B])(fga: Nested[F, G, A]): Nested[F, G, B] =
    Nested(FG.ap(fgf.value)(fga.value))

  override def product[A, B](fga: Nested[F, G, A], fgb: Nested[F, G, B]): Nested[F, G, (A, B)] =
    Nested(FG.product(fga.value, fgb.value))
}

private[data] trait NestedApplicative[F[_], G[_]] extends Applicative[Nested[F, G, ?]] with NestedApply[F, G] {
  def FG: Applicative[λ[α => F[G[α]]]]

  def pure[A](x: A): Nested[F, G, A] = Nested(FG.pure(x))
}

private[data] trait NestedSemigroupK[F[_], G[_]] extends SemigroupK[Nested[F, G, ?]] {
  def FG: SemigroupK[λ[α => F[G[α]]]]

  def combineK[A](x: Nested[F, G, A], y: Nested[F, G, A]): Nested[F, G, A] = Nested(FG.combineK(x.value, y.value))
}

private[data] trait NestedMonoidK[F[_], G[_]] extends MonoidK[Nested[F, G, ?]] with NestedSemigroupK[F, G] {
  def FG: MonoidK[λ[α => F[G[α]]]]

  def empty[A]: Nested[F, G, A] = Nested(FG.empty[A])
}

private[data] trait NestedAlternative[F[_], G[_]] extends Alternative[Nested[F, G, ?]] with NestedApplicative[F, G] with NestedMonoidK[F, G] {
  def FG: Alternative[λ[α => F[G[α]]]]
}

private[data] trait NestedFoldable[F[_], G[_]] extends Foldable[Nested[F, G, ?]] {
  def FG: Foldable[λ[α => F[G[α]]]]

  def foldLeft[A, B](fga: Nested[F, G, A], b: B)(f: (B, A) => B): B =
    FG.foldLeft(fga.value, b)(f)

  def foldRight[A, B](fga: Nested[F, G, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    FG.foldRight(fga.value, lb)(f)
}

private[data] trait NestedTraverse[F[_], G[_]] extends Traverse[Nested[F, G, ?]] with NestedFoldable[F, G] with NestedFunctor[F, G] {
  def FG: Traverse[λ[α => F[G[α]]]]

  override def traverse[H[_]: Applicative, A, B](fga: Nested[F, G, A])(f: A => H[B]): H[Nested[F, G, B]] =
    Applicative[H].map(FG.traverse(fga.value)(f))(Nested(_))
}

private[data] trait NestedReducible[F[_], G[_]] extends Reducible[Nested[F, G, ?]] with NestedFoldable[F, G] {
  def FG: Reducible[λ[α => F[G[α]]]]

  def reduceLeftTo[A, B](fga: Nested[F, G, A])(f: A => B)(g: (B, A) => B): B =
    FG.reduceLeftTo(fga.value)(f)(g)

  def reduceRightTo[A, B](fga: Nested[F, G, A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B] =
    FG.reduceRightTo(fga.value)(f)(g)
}

private[data] trait NestedContravariant[F[_], G[_]] extends Contravariant[Nested[F, G, ?]] {
  def FG: Contravariant[λ[α => F[G[α]]]]

  def contramap[A, B](fga: Nested[F, G, A])(f: B => A): Nested[F, G, B] =
    Nested(FG.contramap(fga.value)(f))
}
