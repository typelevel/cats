package cats
package data

import cats.functor.Contravariant

/**
 * [[Prod]] is a product to two independent functor values.
 *
 * See: [[https://www.cs.ox.ac.uk/jeremy.gibbons/publications/iterator.pdf The Essence of the Iterator Pattern]]
 */
final case class Prod[F[_], G[_], A](first: F[A], second: G[A])

object Prod extends ProdInstances

private[data] sealed abstract class ProdInstances extends ProdInstances0 {
  implicit def catsDataAlternativeForProd[F[_], G[_]](implicit FF: Alternative[F], GG: Alternative[G]): Alternative[λ[α => Prod[F, G, α]]] = new ProdAlternative[F, G] {
    def F: Alternative[F] = FF
    def G: Alternative[G] = GG
  }

  implicit def catsDataEqForProd[F[_], G[_], A](implicit FF: Eq[F[A]], GG: Eq[G[A]]): Eq[Prod[F, G, A]] = new Eq[Prod[F, G, A]] {
    def eqv(x: Prod[F, G, A], y: Prod[F, G, A]): Boolean =
      FF.eqv(x.first, y.first) && GG.eqv(x.second, y.second)
  }
}

private[data] sealed abstract class ProdInstances0 extends ProdInstances1 {
  implicit def catsDataMonoidKForProd[F[_], G[_]](implicit FF: MonoidK[F], GG: MonoidK[G]): MonoidK[λ[α => Prod[F, G, α]]] = new ProdMonoidK[F, G] {
    def F: MonoidK[F] = FF
    def G: MonoidK[G] = GG
  }
}

private[data] sealed abstract class ProdInstances1 extends ProdInstances2 {
  implicit def catsDataSemigroupKForProd[F[_], G[_]](implicit FF: SemigroupK[F], GG: SemigroupK[G]): SemigroupK[λ[α => Prod[F, G, α]]] = new ProdSemigroupK[F, G] {
    def F: SemigroupK[F] = FF
    def G: SemigroupK[G] = GG
  }
}

private[data] sealed abstract class ProdInstances2 extends ProdInstances3 {
  implicit def catsDataApplicativeForProd[F[_], G[_]](implicit FF: Applicative[F], GG: Applicative[G]): Applicative[λ[α => Prod[F, G, α]]] = new ProdApplicative[F, G] {
    def F: Applicative[F] = FF
    def G: Applicative[G] = GG
  }
}

private[data] sealed abstract class ProdInstances3 extends ProdInstances4 {
  implicit def catsDataApplyForProd[F[_], G[_]](implicit FF: Apply[F], GG: Apply[G]): Apply[λ[α => Prod[F, G, α]]] = new ProdApply[F, G] {
    def F: Apply[F] = FF
    def G: Apply[G] = GG
  }
}

private[data] sealed abstract class ProdInstances4 {
  implicit def catsDataFunctorForProd[F[_], G[_]](implicit FF: Functor[F], GG: Functor[G]): Functor[λ[α => Prod[F, G, α]]] = new ProdFunctor[F, G] {
    def F: Functor[F] = FF
    def G: Functor[G] = GG
  }
  implicit def catsDataContravariantForProd[F[_], G[_]](implicit FC: Contravariant[F], GC: Contravariant[G]): Contravariant[λ[α => Prod[F, G, α]]] = new ProdContravariant[F, G] {
    def F: Contravariant[F] = FC
    def G: Contravariant[G] = GC
  }
}

sealed trait ProdFunctor[F[_], G[_]] extends Functor[λ[α => Prod[F, G, α]]] {
  def F: Functor[F]
  def G: Functor[G]
  override def map[A, B](fa: Prod[F, G, A])(f: A => B): Prod[F, G, B] = Prod(F.map(fa.first)(f), G.map(fa.second)(f))
}

sealed trait ProdContravariant[F[_], G[_]] extends Contravariant[λ[α => Prod[F, G, α]]] {
  def F: Contravariant[F]
  def G: Contravariant[G]
  def contramap[A, B](fa: Prod[F, G, A])(f: B => A): Prod[F, G, B] = Prod(F.contramap(fa.first)(f), G.contramap(fa.second)(f))
}

sealed trait ProdApply[F[_], G[_]] extends Apply[λ[α => Prod[F, G, α]]] with ProdFunctor[F, G] {
  def F: Apply[F]
  def G: Apply[G]
  def ap[A, B](f: Prod[F, G, A => B])(fa: Prod[F, G, A]): Prod[F, G, B] =
    Prod(F.ap(f.first)(fa.first), G.ap(f.second)(fa.second))
  override def product[A, B](fa: Prod[F, G, A], fb: Prod[F, G, B]): Prod[F, G, (A, B)] =
    Prod(F.product(fa.first, fb.first), G.product(fa.second, fb.second))
}

sealed trait ProdApplicative[F[_], G[_]] extends Applicative[λ[α => Prod[F, G, α]]] with ProdApply[F, G] {
  def F: Applicative[F]
  def G: Applicative[G]
  def pure[A](a: A): Prod[F, G, A] = Prod(F.pure(a), G.pure(a))
}

sealed trait ProdSemigroupK[F[_], G[_]] extends SemigroupK[λ[α => Prod[F, G, α]]] {
  def F: SemigroupK[F]
  def G: SemigroupK[G]
  override def combineK[A](x: Prod[F, G, A], y: Prod[F, G, A]): Prod[F, G, A] =
    Prod(F.combineK(x.first, y.first), G.combineK(x.second, y.second))
}

sealed trait ProdMonoidK[F[_], G[_]] extends MonoidK[λ[α => Prod[F, G, α]]] with ProdSemigroupK[F, G] {
  def F: MonoidK[F]
  def G: MonoidK[G]
  override def empty[A]: Prod[F, G, A] =
    Prod(F.empty[A], G.empty[A])
}

sealed trait ProdAlternative[F[_], G[_]] extends Alternative[λ[α => Prod[F, G, α]]]
  with ProdApplicative[F, G] with ProdMonoidK[F, G] {
  def F: Alternative[F]
  def G: Alternative[G]
}
