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
  implicit def catsDataEqForNested[F[_], G[_], A](implicit FGA: Eq[F[G[A]]]): Eq[Nested[F, G, A]] =
    FGA.on(_.value)

  implicit def catsDataTraverseForNested[F[_]: Traverse, G[_]: Traverse]: Traverse[Nested[F, G, ?]] =
    new Traverse[Nested[F, G, ?]] {
      val instance = Traverse[F].compose[G]

      def traverse[H[_]: Applicative, A, B](fga: Nested[F, G, A])(f: A => H[B]): H[Nested[F, G, B]] =
        Applicative[H].map(instance.traverse(fga.value)(f))(Nested(_))

      def foldLeft[A, B](fga: Nested[F, G, A], b: B)(f: (B, A) => B): B =
        instance.foldLeft(fga.value, b)(f)

      def foldRight[A, B](fga: Nested[F, G, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        instance.foldRight(fga.value, lb)(f)

      override def map[A, B](fga: Nested[F, G, A])(f: A => B): Nested[F, G, B] =
        Nested(instance.map(fga.value)(f))

      override def imap[A, B](fga: Nested[F, G, A])(f: A => B)(g: B => A): Nested[F, G, B] =
        Nested(instance.imap(fga.value)(f)(g))
    }
}

private[data] sealed abstract class NestedInstances1 extends NestedInstances2 {
  implicit def catsDataReducibleForNested[F[_]: Reducible, G[_]: Reducible]: Reducible[Nested[F, G, ?]] =
    new Reducible[Nested[F, G, ?]] {
      val instance = Reducible[F].compose[G]

      def reduceLeftTo[A, B](fga: Nested[F, G, A])(f: A => B)(g: (B, A) => B): B =
        instance.reduceLeftTo(fga.value)(f)(g)

      def reduceRightTo[A, B](fga: Nested[F, G, A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B] =
        instance.reduceRightTo(fga.value)(f)(g)

      def foldLeft[A, B](fga: Nested[F, G, A], b: B)(f: (B, A) => B): B =
        instance.foldLeft(fga.value, b)(f)

      def foldRight[A, B](fga: Nested[F, G, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        instance.foldRight(fga.value, lb)(f)
    }

  implicit def catsDataContravariantForNested[F[_]: Contravariant, G[_]: Contravariant]: Functor[Nested[F, G, ?]] =
    new Functor[Nested[F, G, ?]] {
      val instance = Contravariant[F].compose[G]

      def map[A, B](fga: Nested[F, G, A])(f: A => B): Nested[F, G, B] =
        Nested(instance.map(fga.value)(f))
    }

}

private[data] sealed abstract class NestedInstances2 extends NestedInstances3 {
  implicit def catsDataFoldableForNested[F[_]: Foldable, G[_]: Foldable]: Foldable[Nested[F, G, ?]] =
    new Foldable[Nested[F, G, ?]] {
      val instance = Foldable[F].compose[G]

      def foldLeft[A, B](fga: Nested[F, G, A], b: B)(f: (B, A) => B): B =
        instance.foldLeft(fga.value, b)(f)

      def foldRight[A, B](fga: Nested[F, G, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        instance.foldRight(fga.value, lb)(f)
    }


  implicit def catsDataContravariantForCovariantNested[F[_]: Contravariant, G[_]: Functor]: Contravariant[Nested[F, G, ?]] =
    new Contravariant[Nested[F, G, ?]] {
      val instance = Contravariant[F].composeFunctor[G]

      def contramap[A, B](fga: Nested[F, G, A])(f: B => A): Nested[F, G, B] =
        Nested(instance.contramap(fga.value)(f))
    }
}

private[data] sealed abstract class NestedInstances3 extends NestedInstances4 {
  implicit def catsDataAlternativeForNested[F[_]: Alternative, G[_]: Applicative]: Alternative[Nested[F, G, ?]] =
    new Alternative[Nested[F, G, ?]] {
      val instance = Alternative[F].compose[G]

      override def imap[A, B](fga: Nested[F, G, A])(f: A => B)(g: B => A): Nested[F, G, B] =
        Nested(instance.imap(fga.value)(f)(g))

      override def map[A, B](fga: Nested[F, G, A])(f: A => B): Nested[F, G, B] =
        Nested(instance.map(fga.value)(f))

      def ap[A, B](fgf: Nested[F, G, A => B])(fga: Nested[F, G, A]): Nested[F, G, B] =
        Nested(instance.ap(fgf.value)(fga.value))

      override def product[A, B](fga: Nested[F, G, A], fgb: Nested[F, G, B]): Nested[F, G, (A, B)] =
        Nested(instance.product(fga.value, fgb.value))

      def pure[A](x: A): Nested[F, G, A] = Nested(instance.pure(x))

      def combineK[A](x: Nested[F, G, A], y: Nested[F, G, A]): Nested[F, G, A] = Nested(instance.combineK(x.value, y.value))

      def empty[A]: Nested[F, G, A] = Nested(instance.empty[A])
    }

  implicit def catsDataContravariantForContravariantNested[F[_]: Functor, G[_]: Contravariant]: Contravariant[Nested[F, G, ?]] =
    new Contravariant[Nested[F, G, ?]] {
      val instance = Functor[F].composeContravariant[G]

      def contramap[A, B](fga: Nested[F, G, A])(f: B => A): Nested[F, G, B] =
        Nested(instance.contramap(fga.value)(f))
    }
}

private[data] sealed abstract class NestedInstances4 extends NestedInstances5 {
  implicit def catsDataApplicativeForNested[F[_]: Applicative, G[_]: Applicative]: Applicative[Nested[F, G, ?]] =
    new Applicative[Nested[F, G, ?]] {
      val instance = Applicative[F].compose[G]

      override def imap[A, B](fga: Nested[F, G, A])(f: A => B)(g: B => A): Nested[F, G, B] =
        Nested(instance.imap(fga.value)(f)(g))

      override def map[A, B](fga: Nested[F, G, A])(f: A => B): Nested[F, G, B] =
        Nested(instance.map(fga.value)(f))

      def ap[A, B](fgf: Nested[F, G, A => B])(fga: Nested[F, G, A]): Nested[F, G, B] =
        Nested(instance.ap(fgf.value)(fga.value))

      override def product[A, B](fga: Nested[F, G, A], fgb: Nested[F, G, B]): Nested[F, G, (A, B)] =
        Nested(instance.product(fga.value, fgb.value))

      def pure[A](x: A): Nested[F, G, A] = Nested(instance.pure(x))
    }

  implicit def catsDataMonoidKForNested[F[_]: MonoidK, G[_]]: MonoidK[Nested[F, G, ?]] =
    new MonoidK[Nested[F, G, ?]] {
      val instance = MonoidK[F].compose[G]

      def combineK[A](x: Nested[F, G, A], y: Nested[F, G, A]): Nested[F, G, A] = Nested(instance.combineK(x.value, y.value))

      def empty[A]: Nested[F, G, A] = Nested(instance.empty[A])
    }
}

private[data] sealed abstract class NestedInstances5 extends NestedInstances6 {
  implicit def catsDataApplyForNested[F[_]: Apply, G[_]: Apply]: Apply[Nested[F, G, ?]] =
    new Apply[Nested[F, G, ?]] {
      val instance = Apply[F].compose[G]

      def ap[A, B](fgf: Nested[F, G, A => B])(fga: Nested[F, G, A]): Nested[F, G, B] =
        Nested(instance.ap(fgf.value)(fga.value))

      override def product[A, B](fga: Nested[F, G, A], fgb: Nested[F, G, B]): Nested[F, G, (A, B)] =
        Nested(instance.product(fga.value, fgb.value))

      def map[A, B](fga: Nested[F, G, A])(f: A => B): Nested[F, G, B] =
        Nested(instance.map(fga.value)(f))
    }

  implicit def catsDataSemigroupKForNested[F[_]: SemigroupK, G[_]]: SemigroupK[Nested[F, G, ?]] =
    new SemigroupK[Nested[F, G, ?]] {
      val instance = SemigroupK[F].compose[G]

      def combineK[A](x: Nested[F, G, A], y: Nested[F, G, A]): Nested[F, G, A] = Nested(instance.combineK(x.value, y.value))
    }
}

private[data] sealed abstract class NestedInstances6 extends NestedInstances7 {
  implicit def catsDataFunctorForNested[F[_]: Functor, G[_]: Functor]: Functor[Nested[F, G, ?]] =
    new Functor[Nested[F, G, ?]] {
      val instance = Functor[F].compose[G]

      def map[A, B](fga: Nested[F, G, A])(f: A => B): Nested[F, G, B] =
        Nested(instance.map(fga.value)(f))

      override def imap[A, B](fga: Nested[F, G, A])(f: A => B)(g: B => A): Nested[F, G, B] =
        Nested(instance.imap(fga.value)(f)(g))
    }
}

private[data] sealed abstract class NestedInstances7 extends NestedInstances8 {
  implicit def catsDataInvariantForNested[F[_]: Invariant, G[_]: Invariant]: Invariant[Nested[F, G, ?]] =
    new Invariant[Nested[F, G, ?]] {
      val instance = Invariant[F].compose[G]

      def imap[A, B](fga: Nested[F, G, A])(f: A => B)(g: B => A): Nested[F, G, B] =
        Nested(instance.imap(fga.value)(f)(g))
    }
}

private[data] sealed abstract class NestedInstances8 extends NestedInstances9 {
  implicit def catsDataInvariantForCovariantNested[F[_]: Invariant, G[_]: Functor]: Invariant[Nested[F, G, ?]] =
    new Invariant[Nested[F, G, ?]] {
      val instance = Invariant[F].composeFunctor[G]

      def imap[A, B](fga: Nested[F, G, A])(f: A => B)(g: B => A): Nested[F, G, B] =
        Nested(instance.imap(fga.value)(f)(g))
    }
}

private[data] sealed abstract class NestedInstances9 {
  implicit def catsDataInvariantForNestedContravariant[F[_]: Invariant, G[_]: Contravariant]: Invariant[Nested[F, G, ?]] =
    new Invariant[Nested[F, G, ?]] {
      val instance = Invariant[F].composeContravariant[G]

      def imap[A, B](fga: Nested[F, G, A])(f: A => B)(g: B => A): Nested[F, G, B] =
        Nested(instance.imap(fga.value)(f)(g))
    }
}
