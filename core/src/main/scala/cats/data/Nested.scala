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
    new Traverse[Nested[F, G, ?]] {
      val instance = Traverse[F].nest[G]

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
  implicit def nestedReducible[F[_]: Reducible, G[_]: Reducible]: Reducible[Nested[F, G, ?]] =
    new Reducible[Nested[F, G, ?]] {
      val instance = Reducible[F].nest[G]

      def reduceLeftTo[A, B](fga: Nested[F, G, A])(f: A => B)(g: (B, A) => B): B =
        instance.reduceLeftTo(fga.value)(f)(g)

      def reduceRightTo[A, B](fga: Nested[F, G, A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B] =
        instance.reduceRightTo(fga.value)(f)(g)

      def foldLeft[A, B](fga: Nested[F, G, A], b: B)(f: (B, A) => B): B =
        instance.foldLeft(fga.value, b)(f)

      def foldRight[A, B](fga: Nested[F, G, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        instance.foldRight(fga.value, lb)(f)
    }

  implicit def nestedContravariant[F[_]: Contravariant, G[_]: Contravariant]: Functor[Nested[F, G, ?]] =
    new Functor[Nested[F, G, ?]] {
      val instance = Contravariant[F].nest[G]

      def map[A, B](fga: Nested[F, G, A])(f: A => B): Nested[F, G, B] =
        Nested(instance.map(fga.value)(f))
    }

}

private[data] sealed abstract class NestedInstances2 extends NestedInstances3 {
  implicit def nestedFoldable[F[_]: Foldable, G[_]: Foldable]: Foldable[Nested[F, G, ?]] =
    new Foldable[Nested[F, G, ?]] {
      val instance = Foldable[F].nest[G]

      def foldLeft[A, B](fga: Nested[F, G, A], b: B)(f: (B, A) => B): B =
        instance.foldLeft(fga.value, b)(f)

      def foldRight[A, B](fga: Nested[F, G, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        instance.foldRight(fga.value, lb)(f)
    }


  implicit def nestedContravariantCovariant[F[_]: Contravariant, G[_]: Functor]: Contravariant[Nested[F, G, ?]] =
    new Contravariant[Nested[F, G, ?]] {
      val instance = Contravariant[F].nestFunctor[G]

      def contramap[A, B](fga: Nested[F, G, A])(f: B => A): Nested[F, G, B] =
        Nested(instance.contramap(fga.value)(f))
    }
}

private[data] sealed abstract class NestedInstances3 extends NestedInstances4 {
  implicit def nestedAlternative[F[_]: Alternative, G[_]: Applicative]: Alternative[Nested[F, G, ?]] =
    new Alternative[Nested[F, G, ?]] {
      val instance = Alternative[F].nest[G]

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

  implicit def nestedCovariantContravariant[F[_]: Functor, G[_]: Contravariant]: Contravariant[Nested[F, G, ?]] =
    new Contravariant[Nested[F, G, ?]] {
      val instance = Functor[F].nestContravariant[G]

      def contramap[A, B](fga: Nested[F, G, A])(f: B => A): Nested[F, G, B] =
        Nested(instance.contramap(fga.value)(f))
    }
}

private[data] sealed abstract class NestedInstances4 extends NestedInstances5 {
  implicit def nestedApplicative[F[_]: Applicative, G[_]: Applicative]: Applicative[Nested[F, G, ?]] =
    new Applicative[Nested[F, G, ?]] {
      val instance = Applicative[F].nest[G]

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

  implicit def nestedMonoidK[F[_]: MonoidK, G[_]]: MonoidK[Nested[F, G, ?]] =
    new MonoidK[Nested[F, G, ?]] {
      val instance = MonoidK[F].nest[G]

      def combineK[A](x: Nested[F, G, A], y: Nested[F, G, A]): Nested[F, G, A] = Nested(instance.combineK(x.value, y.value))

      def empty[A]: Nested[F, G, A] = Nested(instance.empty[A])
    }
}

private[data] sealed abstract class NestedInstances5 extends NestedInstances6 {
  implicit def nestedApply[F[_]: Apply, G[_]: Apply]: Apply[Nested[F, G, ?]] =
    new Apply[Nested[F, G, ?]] {
      val instance = Apply[F].nest[G]

      def ap[A, B](fgf: Nested[F, G, A => B])(fga: Nested[F, G, A]): Nested[F, G, B] =
        Nested(instance.ap(fgf.value)(fga.value))

      override def product[A, B](fga: Nested[F, G, A], fgb: Nested[F, G, B]): Nested[F, G, (A, B)] =
        Nested(instance.product(fga.value, fgb.value))

      def map[A, B](fga: Nested[F, G, A])(f: A => B): Nested[F, G, B] =
        Nested(instance.map(fga.value)(f))
    }

  implicit def nestedSemigroupK[F[_]: SemigroupK, G[_]]: SemigroupK[Nested[F, G, ?]] =
    new SemigroupK[Nested[F, G, ?]] {
      val instance = SemigroupK[F].nest[G]

      def combineK[A](x: Nested[F, G, A], y: Nested[F, G, A]): Nested[F, G, A] = Nested(instance.combineK(x.value, y.value))
    }
}

private[data] sealed abstract class NestedInstances6 extends NestedInstances7 {
  implicit def nestedFunctor[F[_]: Functor, G[_]: Functor]: Functor[Nested[F, G, ?]] =
    new Functor[Nested[F, G, ?]] {
      val instance = Functor[F].nest[G]

      def map[A, B](fga: Nested[F, G, A])(f: A => B): Nested[F, G, B] =
        Nested(instance.map(fga.value)(f))

      override def imap[A, B](fga: Nested[F, G, A])(f: A => B)(g: B => A): Nested[F, G, B] =
        Nested(instance.imap(fga.value)(f)(g))
    }
}

private[data] sealed abstract class NestedInstances7 extends NestedInstances8 {
  implicit def nestedInvariant[F[_]: Invariant, G[_]: Invariant]: Invariant[Nested[F, G, ?]] =
    new Invariant[Nested[F, G, ?]] {
      val instance = Invariant[F].nest[G]

      def imap[A, B](fga: Nested[F, G, A])(f: A => B)(g: B => A): Nested[F, G, B] =
        Nested(instance.imap(fga.value)(f)(g))
    }
}

private[data] sealed abstract class NestedInstances8 extends NestedInstances9 {
  implicit def nestedInvariantCovariant[F[_]: Invariant, G[_]: Functor]: Invariant[Nested[F, G, ?]] =
    new Invariant[Nested[F, G, ?]] {
      val instance = Invariant[F].nestFunctor[G]

      def imap[A, B](fga: Nested[F, G, A])(f: A => B)(g: B => A): Nested[F, G, B] =
        Nested(instance.imap(fga.value)(f)(g))
    }
}

private[data] sealed abstract class NestedInstances9 {
  implicit def nestedInvariantContravariant[F[_]: Invariant, G[_]: Contravariant]: Invariant[Nested[F, G, ?]] =
    new Invariant[Nested[F, G, ?]] {
      val instance = Invariant[F].nestContravariant[G]

      def imap[A, B](fga: Nested[F, G, A])(f: A => B)(g: B => A): Nested[F, G, B] =
        Nested(instance.imap(fga.value)(f)(g))
    }
}

/********************
** Implementations **
********************/

private[cats] trait NestedInvariant[F[_], G[_]] extends Invariant[Lambda[A => F[G[A]]]] { outer =>
  def F: Invariant[F]
  def G: Invariant[G]

  override def imap[A, B](fga: F[G[A]])(f: A => B)(g: B => A): F[G[B]] =
    F.imap(fga)(ga => G.imap(ga)(f)(g))(gb => G.imap(gb)(g)(f))
}

private[cats] trait NestedFunctor[F[_], G[_]] extends Functor[Lambda[A => F[G[A]]]] with NestedInvariant[F, G] { outer =>
  def F: Functor[F]
  def G: Functor[G]

  override def map[A, B](fga: F[G[A]])(f: A => B): F[G[B]] =
    F.map(fga)(ga => G.map(ga)(f))
}

private[cats] trait NestedApply[F[_], G[_]] extends Apply[Lambda[A => F[G[A]]]] with NestedFunctor[F, G] { outer =>
  def F: Apply[F]
  def G: Apply[G]

  override def ap[A, B](fgf: F[G[A => B]])(fga: F[G[A]]): F[G[B]] =
    F.ap(F.map(fgf)(gf => G.ap(gf)(_)))(fga)

  override def product[A, B](fga: F[G[A]], fgb: F[G[B]]): F[G[(A, B)]] =
    F.map2(fga, fgb)(G.product)
}

private[cats] trait NestedApplicative[F[_], G[_]] extends Applicative[Lambda[A => F[G[A]]]] with NestedApply[F, G] { outer =>
  def F: Applicative[F]
  def G: Applicative[G]

  override def pure[A](x: A): F[G[A]] = F.pure(G.pure(x))
}

private[cats] trait NestedSemigroupK[F[_], G[_]] extends SemigroupK[Lambda[A => F[G[A]]]] { outer =>
  def F: SemigroupK[F]

  override def combineK[A](x: F[G[A]], y: F[G[A]]): F[G[A]] = F.combineK(x, y)
}

private[cats] trait NestedMonoidK[F[_], G[_]] extends MonoidK[Lambda[A => F[G[A]]]] with NestedSemigroupK[F, G] { outer =>
  def F: MonoidK[F]

  override def empty[A]: F[G[A]] = F.empty
}

private[cats] trait NestedAlternative[F[_], G[_]] extends Alternative[Lambda[A => F[G[A]]]] with NestedApplicative[F, G] with NestedMonoidK[F, G] { outer =>
  def F: Alternative[F]
}

private[cats] trait NestedFoldable[F[_], G[_]] extends Foldable[Lambda[A => F[G[A]]]] { outer =>
  def F: Foldable[F]
  def G: Foldable[G]

  override def foldLeft[A, B](fga: F[G[A]], b: B)(f: (B, A) => B): B =
    F.foldLeft(fga, b)((b, a) => G.foldLeft(a, b)(f))

  override def foldRight[A, B](fga: F[G[A]], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    F.foldRight(fga, lb)((ga, lb) => G.foldRight(ga, lb)(f))
}

private[cats] trait NestedTraverse[F[_], G[_]] extends Traverse[Lambda[A => F[G[A]]]] with NestedFoldable[F, G] with NestedFunctor[F, G] { outer =>
  def F: Traverse[F]
  def G: Traverse[G]

  override def traverse[H[_]: Applicative, A, B](fga: F[G[A]])(f: A => H[B]): H[F[G[B]]] =
    F.traverse(fga)(ga => G.traverse(ga)(f))
}

private[cats] trait NestedReducible[F[_], G[_]] extends Reducible[Lambda[A => F[G[A]]]] with NestedFoldable[F, G] { outer =>
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

private[cats] trait NestedContravariant[F[_], G[_]] extends Functor[Lambda[A => F[G[A]]]] { outer =>
  def F: Contravariant[F]
  def G: Contravariant[G]

  override def map[A, B](fga: F[G[A]])(f: A => B): F[G[B]] =
    F.contramap(fga)(gb => G.contramap(gb)(f))
}

private[cats] trait NestedContravariantCovariant[F[_], G[_]] extends Contravariant[Lambda[A => F[G[A]]]] { outer =>
  def F: Contravariant[F]
  def G: Functor[G]

  override def contramap[A, B](fga: F[G[A]])(f: B => A): F[G[B]] =
    F.contramap(fga)(gb => G.map(gb)(f))
}

private[cats] trait NestedCovariantContravariant[F[_], G[_]] extends Contravariant[Lambda[A => F[G[A]]]] { outer =>
  def F: Functor[F]
  def G: Contravariant[G]

  override def contramap[A, B](fga: F[G[A]])(f: B => A): F[G[B]] =
    F.map(fga)(ga => G.contramap(ga)(f))
}

private[cats] trait NestedInvariantCovariant[F[_], G[_]] extends Invariant[Lambda[A => F[G[A]]]] { outer =>
  def F: Invariant[F]
  def G: Functor[G]

  override def imap[A, B](fga: F[G[A]])(f: A => B)(g: B => A): F[G[B]] =
    F.imap(fga)(ga => G.map(ga)(f))(gb => G.map(gb)(g))
}

private[cats] trait NestedInvariantContravariant[F[_], G[_]] extends Invariant[Lambda[A => F[G[A]]]] { outer =>
  def F: Invariant[F]
  def G: Contravariant[G]

  override def imap[A, B](fga: F[G[A]])(f: A => B)(g: B => A): F[G[B]] =
    F.imap(fga)(ga => G.contramap(ga)(g))(gb => G.contramap(gb)(f))
}
