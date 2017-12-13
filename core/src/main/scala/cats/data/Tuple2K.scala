package cats
package data

import cats.Contravariant

/**
 * [[Tuple2K]] is a product to two independent functor values.
 *
 * See: [[https://www.cs.ox.ac.uk/jeremy.gibbons/publications/iterator.pdf The Essence of the Iterator Pattern]]
 */
final case class Tuple2K[F[_], G[_], A](first: F[A], second: G[A]) {

  /**
   * Modify the context `G` of `second` using transformation `f`.
   */
  def mapK[H[_]](f: G ~> H): Tuple2K[F, H, A] =
    Tuple2K(first, f(second))

}

object Tuple2K extends Tuple2KInstances

private[data] sealed abstract class Tuple2KInstances extends Tuple2KInstances0 {
  implicit def catsDataOrderForTuple2K[F[_], G[_], A](implicit FF: Order[F[A]], GF: Order[G[A]]): Order[Tuple2K[F, G, A]] = new Tuple2KOrder[F, G, A] {
    def F: Order[F[A]] = FF
    def G: Order[G[A]] = GF
  }
  implicit def catsDataShowForTuple2K[F[_], G[_], A](implicit FF: Show[F[A]], GF: Show[G[A]]): Show[Tuple2K[F, G, A]] = new Tuple2KShow[F, G, A] {
    def F: Show[F[A]] = FF
    def G: Show[G[A]] = GF
  }
  implicit def catsDataContravariantMonoidalForTuple2k[F[_], G[_]](implicit FD: ContravariantMonoidal[F], GD: ContravariantMonoidal[G]): ContravariantMonoidal[λ[α => Tuple2K[F, G, α]]] =
    new Tuple2KContravariantMonoidal[F, G] {
      def F: ContravariantMonoidal[F] = FD
      def G: ContravariantMonoidal[G] = GD
    }
}

private[data] sealed abstract class Tuple2KInstances0 extends Tuple2KInstances1 {
  implicit def catsDataTraverseForTuple2K[F[_], G[_]](implicit FF: Traverse[F], GF: Traverse[G]): Traverse[λ[α => Tuple2K[F, G, α]]] = new Tuple2KTraverse[F, G] {
    def F: Traverse[F] = FF
    def G: Traverse[G] = GF
  }
  implicit def catsDataContravariantForTuple2K[F[_], G[_]](implicit FC: Contravariant[F], GC: Contravariant[G]): Contravariant[λ[α => Tuple2K[F, G, α]]] = new Tuple2KContravariant[F, G] {
    def F: Contravariant[F] = FC
    def G: Contravariant[G] = GC
  }
  implicit def catsDataEqForTuple2K[F[_], G[_], A](implicit FF: Eq[F[A]], GG: Eq[G[A]]): Eq[Tuple2K[F, G, A]] = new Eq[Tuple2K[F, G, A]] {
    def eqv(x: Tuple2K[F, G, A], y: Tuple2K[F, G, A]): Boolean =
      FF.eqv(x.first, y.first) && GG.eqv(x.second, y.second)
  }
}

private[data] sealed abstract class Tuple2KInstances1 extends Tuple2KInstances2 {
  implicit def catsDataAlternativeForTuple2K[F[_], G[_]](implicit FF: Alternative[F], GG: Alternative[G]): Alternative[λ[α => Tuple2K[F, G, α]]] = new Tuple2KAlternative[F, G] {
    def F: Alternative[F] = FF
    def G: Alternative[G] = GG
  }
  implicit def catsDataFoldableForTuple2K[F[_], G[_]](implicit FF: Foldable[F], GF: Foldable[G]): Foldable[λ[α => Tuple2K[F, G, α]]] = new Tuple2KFoldable[F, G] {
    def F: Foldable[F] = FF
    def G: Foldable[G] = GF
  }
}

private[data] sealed abstract class Tuple2KInstances2 extends Tuple2KInstances3 {
  implicit def catsDataMonadForTuple2K[F[_], G[_]](implicit FM: Monad[F], GM: Monad[G]): Monad[λ[α => Tuple2K[F, G, α]]] = new Tuple2KMonad[F, G] {
    def F: Monad[F] = FM
    def G: Monad[G] = GM
  }
  implicit def catsDataMonoidKForTuple2K[F[_], G[_]](implicit FF: MonoidK[F], GG: MonoidK[G]): MonoidK[λ[α => Tuple2K[F, G, α]]] = new Tuple2KMonoidK[F, G] {
    def F: MonoidK[F] = FF
    def G: MonoidK[G] = GG
  }
}

private[data] sealed abstract class Tuple2KInstances3 extends Tuple2KInstances4 {
  implicit def catsDataCommutativeApplicativeForTuple2K[F[_], G[_]](implicit FF: CommutativeApplicative[F], GG: CommutativeApplicative[G]): CommutativeApplicative[λ[α => Tuple2K[F, G, α]]] =
    new Tuple2KApplicative[F, G] with CommutativeApplicative[λ[α => Tuple2K[F, G, α]]] {
      def F: Applicative[F] = FF
      def G: Applicative[G] = GG
    }
}

private[data] sealed abstract class Tuple2KInstances4 extends Tuple2KInstances5 {
  implicit def catsDataSemigroupKForTuple2K[F[_], G[_]](implicit FF: SemigroupK[F], GG: SemigroupK[G]): SemigroupK[λ[α => Tuple2K[F, G, α]]] = new Tuple2KSemigroupK[F, G] {
    def F: SemigroupK[F] = FF
    def G: SemigroupK[G] = GG
  }
  implicit def catsDataCommutativeApplyForTuple2K[F[_], G[_]](implicit FF: CommutativeApply[F], GG: CommutativeApply[G]): CommutativeApply[λ[α => Tuple2K[F, G, α]]] =
    new Tuple2KApply[F, G] with CommutativeApply[λ[α => Tuple2K[F, G, α]]] {
      def F: Apply[F] = FF
      def G: Apply[G] = GG
    }
}

private[data] sealed abstract class Tuple2KInstances5 extends Tuple2KInstances6 {
  implicit def catsDataApplicativeForTuple2K[F[_], G[_]](implicit FF: Applicative[F], GG: Applicative[G]): Applicative[λ[α => Tuple2K[F, G, α]]] = new Tuple2KApplicative[F, G] {
    def F: Applicative[F] = FF
    def G: Applicative[G] = GG
  }
}


private[data] sealed abstract class Tuple2KInstances6 extends Tuple2KInstances7 {
  implicit def catsDataApplyForTuple2K[F[_], G[_]](implicit FF: Apply[F], GG: Apply[G]): Apply[λ[α => Tuple2K[F, G, α]]] = new Tuple2KApply[F, G] {
    def F: Apply[F] = FF
    def G: Apply[G] = GG
  }
}

private[data] sealed abstract class Tuple2KInstances7 extends Tuple2KInstances8 {
  implicit def catsDataDistributiveForTuple2K[F[_], G[_]](implicit FF: Distributive[F], GG: Distributive[G]): Distributive[λ[α => Tuple2K[F, G, α]]] = new Tuple2KDistributive[F, G] {
    def F: Distributive[F] = FF
    def G: Distributive[G] = GG
  }
}

private[data] sealed abstract class Tuple2KInstances8 {
  implicit def catsDataFunctorForTuple2K[F[_], G[_]](implicit FF: Functor[F], GG: Functor[G]): Functor[λ[α => Tuple2K[F, G, α]]] = new Tuple2KFunctor[F, G] {
    def F: Functor[F] = FF
    def G: Functor[G] = GG
  }
}

private[data] sealed trait Tuple2KFunctor[F[_], G[_]] extends Functor[λ[α => Tuple2K[F, G, α]]] {
  def F: Functor[F]
  def G: Functor[G]
  override def map[A, B](fa: Tuple2K[F, G, A])(f: A => B): Tuple2K[F, G, B] = Tuple2K(F.map(fa.first)(f), G.map(fa.second)(f))
}


private[data] sealed trait Tuple2KDistributive[F[_], G[_]] extends Distributive[λ[α => Tuple2K[F, G, α]]] {
  def F: Distributive[F]
  def G: Distributive[G]
  override def distribute[H[_]: Functor, A, B](ha: H[A])(f: A => Tuple2K[F, G, B]): Tuple2K[F, G, H[B]] = Tuple2K(F.distribute(ha){a => f(a).first}, G.distribute(ha){a => f(a).second})
  override def map[A, B](fa: Tuple2K[F, G, A])(f: A => B): Tuple2K[F, G, B] = Tuple2K(F.map(fa.first)(f), G.map(fa.second)(f))
}

private[data] sealed trait Tuple2KContravariant[F[_], G[_]] extends Contravariant[λ[α => Tuple2K[F, G, α]]] {
  def F: Contravariant[F]
  def G: Contravariant[G]
  def contramap[A, B](fa: Tuple2K[F, G, A])(f: B => A): Tuple2K[F, G, B] = Tuple2K(F.contramap(fa.first)(f), G.contramap(fa.second)(f))
}

private[data] sealed trait Tuple2KContravariantMonoidal[F[_], G[_]] extends ContravariantMonoidal[λ[α => Tuple2K[F, G, α]]] {
  def F: ContravariantMonoidal[F]
  def G: ContravariantMonoidal[G]
  def unit[A]: Tuple2K[F, G, A] = Tuple2K(F.unit, G.unit)
  def product[A, B](fa: Tuple2K[F, G, A], fb: Tuple2K[F, G, B]): Tuple2K[F, G, (A, B)] =
    Tuple2K(F.product(fa.first, fb.first), G.product(fa.second, fb.second))
  def contramap[A, B](fa: Tuple2K[F, G, A])(f: B => A): Tuple2K[F, G, B] =
    Tuple2K(F.contramap(fa.first)(f), G.contramap(fa.second)(f))
}

private[data] sealed trait Tuple2KApply[F[_], G[_]] extends Apply[λ[α => Tuple2K[F, G, α]]] with Tuple2KFunctor[F, G] {
  def F: Apply[F]
  def G: Apply[G]
  override def ap[A, B](f: Tuple2K[F, G, A => B])(fa: Tuple2K[F, G, A]): Tuple2K[F, G, B] =
    Tuple2K(F.ap(f.first)(fa.first), G.ap(f.second)(fa.second))
  override def product[A, B](fa: Tuple2K[F, G, A], fb: Tuple2K[F, G, B]): Tuple2K[F, G, (A, B)] =
    Tuple2K(F.product(fa.first, fb.first), G.product(fa.second, fb.second))
  override def map2Eval[A, B, Z](fa: Tuple2K[F, G, A], fb: Eval[Tuple2K[F, G, B]])(f: (A, B) => Z): Eval[Tuple2K[F, G, Z]] = {
    val fbmemo = fb.memoize // don't recompute this twice internally
    for {
      fz <- F.map2Eval(fa.first, fbmemo.map(_.first))(f)
      gz <- G.map2Eval(fa.second, fbmemo.map(_.second))(f)
    } yield Tuple2K(fz, gz)
  }
}

private[data] sealed trait Tuple2KApplicative[F[_], G[_]] extends Applicative[λ[α => Tuple2K[F, G, α]]] with Tuple2KApply[F, G] {
  def F: Applicative[F]
  def G: Applicative[G]
  def pure[A](a: A): Tuple2K[F, G, A] = Tuple2K(F.pure(a), G.pure(a))
}

private[data] sealed trait Tuple2KSemigroupK[F[_], G[_]] extends SemigroupK[λ[α => Tuple2K[F, G, α]]] {
  def F: SemigroupK[F]
  def G: SemigroupK[G]
  override def combineK[A](x: Tuple2K[F, G, A], y: Tuple2K[F, G, A]): Tuple2K[F, G, A] =
    Tuple2K(F.combineK(x.first, y.first), G.combineK(x.second, y.second))
}

private[data] sealed trait Tuple2KMonoidK[F[_], G[_]] extends MonoidK[λ[α => Tuple2K[F, G, α]]] with Tuple2KSemigroupK[F, G] {
  def F: MonoidK[F]
  def G: MonoidK[G]
  override def empty[A]: Tuple2K[F, G, A] =
    Tuple2K(F.empty[A], G.empty[A])
}

private[data] sealed trait Tuple2KAlternative[F[_], G[_]] extends Alternative[λ[α => Tuple2K[F, G, α]]]
  with Tuple2KApplicative[F, G] with Tuple2KMonoidK[F, G] {
  def F: Alternative[F]
  def G: Alternative[G]
}

private[data] sealed trait Tuple2KMonad[F[_], G[_]] extends Monad[λ[α => Tuple2K[F, G, α]]] with Tuple2KApplicative[F, G] {
  def F: Monad[F]
  def G: Monad[G]
  override def pure[A](a: A): Tuple2K[F, G, A] =
    Tuple2K(F.pure(a), G.pure(a))

  override def flatMap[A, B](p: Tuple2K[F, G, A])(f: A => Tuple2K[F, G, B]): Tuple2K[F, G, B] =
    Tuple2K(F.flatMap(p.first)(f(_).first), G.flatMap(p.second)(f(_).second))

  def tailRecM[A, B](a: A)(f: A => Tuple2K[F, G, Either[A, B]]): Tuple2K[F, G, B] =
    Tuple2K(F.tailRecM(a)(f(_).first), G.tailRecM(a)(f(_).second))
}

private[data] sealed trait Tuple2KFoldable[F[_], G[_]] extends Foldable[λ[α => Tuple2K[F, G, α]]] {
  def F: Foldable[F]
  def G: Foldable[G]

  override def foldLeft[A, B](fa: Tuple2K[F, G, A], b: B)(f: (B, A) => B): B =
    G.foldLeft(fa.second, F.foldLeft(fa.first, b)(f))(f)

  override def foldRight[A, B](fa: Tuple2K[F, G, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    F.foldRight(fa.first, G.foldRight(fa.second, lb)(f))(f)
}

private[data] sealed trait Tuple2KTraverse[F[_], G[_]] extends Traverse[λ[α => Tuple2K[F, G, α]]] with Tuple2KFoldable[F, G] {
  def F: Traverse[F]
  def G: Traverse[G]

  override def traverse[H[_], A, B](fa: Tuple2K[F, G, A])(f: A => H[B])(implicit H: Applicative[H]): H[Tuple2K[F, G, B]] =
    H.map2(F.traverse(fa.first)(f), G.traverse(fa.second)(f))(Tuple2K(_, _))
}

private[data] sealed trait Tuple2KShow[F[_], G[_], A] extends Show[Tuple2K[F, G, A]] {
  def F: Show[F[A]]
  def G: Show[G[A]]

  def show(tuple: Tuple2K[F, G, A]): String = s"Tuple2K(${F.show(tuple.first)}, ${G.show(tuple.second)})"
}

private[data] sealed trait Tuple2KOrder[F[_], G[_], A] extends Order[Tuple2K[F, G, A]] {
  def F: Order[F[A]]
  def G: Order[G[A]]

  def compare(x: Tuple2K[F, G, A], y: Tuple2K[F, G, A]): Int =
    Array(F.compare(x.first, y.first), G.compare(x.second, y.second)).find(_ != 0).getOrElse(0)
}
