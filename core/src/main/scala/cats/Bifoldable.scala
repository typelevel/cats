package cats

import simulacrum.typeclass
import scala.annotation.implicitNotFound

/**
 * A type class abstracting over types that give rise to two independent [[cats.Foldable]]s.
 */
@implicitNotFound("Could not find an instance of Bifoldable for ${F}")
@typeclass trait Bifoldable[F[_, _]] extends Serializable { self =>

  /**
   * Collapse the structure with a left-associative function */
  def bifoldLeft[A, B, C](fab: F[A, B], c: C)(f: (C, A) => C, g: (C, B) => C): C

  /**
   * Collapse the structure with a right-associative function */
  def bifoldRight[A, B, C](fab: F[A, B], c: Eval[C])(f: (A, Eval[C]) => Eval[C], g: (B, Eval[C]) => Eval[C]): Eval[C]

  /**
   * Collapse the structure by mapping each element to an element of a type that has a [[cats.Monoid]] */
  def bifoldMap[A, B, C](fab: F[A, B])(f: A => C, g: B => C)(implicit C: Monoid[C]): C =
    bifoldLeft(fab, C.empty)(
      (c: C, a: A) => C.combine(c, f(a)),
      (c: C, b: B) => C.combine(c, g(b))
    )

  def compose[G[_, _]](implicit ev: Bifoldable[G]): Bifoldable[λ[(α, β) => F[G[α, β], G[α, β]]]] =
    new ComposedBifoldable[F, G] {
      val F = self
      val G = ev
    }

  def bifold[A, B](fab: F[A, B])(implicit A: Monoid[A], B: Monoid[B]): (A, B) = {
    import cats.instances.tuple._
    bifoldMap(fab)((_, B.empty), (A.empty, _))
  }
}

object Bifoldable {
  implicit def catsBitraverseForEither: Bitraverse[Either] = cats.instances.either.catsStdBitraverseForEither
  implicit def catsBitraverseForTuple2: Bitraverse[Tuple2] = cats.instances.tuple.catsStdBitraverseForTuple2

  /* ======================================================================== */
  /* THE FOLLOWING CODE IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!!      */
  /* ======================================================================== */

  /**
   * Summon an instance of [[Bifoldable]] for `F`.
   */
  @inline def apply[F[_, _]](implicit instance: Bifoldable[F]): Bifoldable[F] = instance

  trait Ops[F[_, _], A, B] extends Serializable {
    type TypeClassType <: Bifoldable[F]
    def self: F[A, B]
    val typeClassInstance: TypeClassType
    def bifoldLeft[C](c: C)(f: (C, A) => C, g: (C, B) => C): C = typeClassInstance.bifoldLeft[A, B, C](self, c)(f, g)
    def bifoldRight[C](c: Eval[C])(f: (A, Eval[C]) => Eval[C], g: (B, Eval[C]) => Eval[C]): Eval[C] =
      typeClassInstance.bifoldRight[A, B, C](self, c)(f, g)
    def bifoldMap[C](f: A => C, g: B => C)(implicit C: Monoid[C]): C =
      typeClassInstance.bifoldMap[A, B, C](self)(f, g)(C)
    def bifold(implicit A: Monoid[A], B: Monoid[B]): (A, B) = typeClassInstance.bifold[A, B](self)(A, B)
  }
  trait AllOps[F[_, _], A, B] extends Ops[F, A, B]
  trait ToBifoldableOps extends Serializable {
    implicit def toBifoldableOps[F[_, _], A, B](target: F[A, B])(implicit tc: Bifoldable[F]): Ops[F, A, B] {
      type TypeClassType = Bifoldable[F]
    } =
      new Ops[F, A, B] {
        type TypeClassType = Bifoldable[F]
        val self: F[A, B] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToBifoldableOps
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllBifoldableOps[F[_, _], A, B](target: F[A, B])(implicit tc: Bifoldable[F]): AllOps[F, A, B] {
      type TypeClassType = Bifoldable[F]
    } =
      new AllOps[F, A, B] {
        type TypeClassType = Bifoldable[F]
        val self: F[A, B] = target
        val typeClassInstance: TypeClassType = tc
      }
  }

  /* ======================================================================== */
  /* END OF SIMULACRUM-MANAGED CODE                                           */
  /* ======================================================================== */

}

private[cats] trait ComposedBifoldable[F[_, _], G[_, _]] extends Bifoldable[λ[(α, β) => F[G[α, β], G[α, β]]]] {
  implicit def F: Bifoldable[F]
  implicit def G: Bifoldable[G]

  override def bifoldLeft[A, B, C](fab: F[G[A, B], G[A, B]], c: C)(f: (C, A) => C, g: (C, B) => C): C =
    F.bifoldLeft(fab, c)(
      (c: C, gab: G[A, B]) => G.bifoldLeft(gab, c)(f, g),
      (c: C, gab: G[A, B]) => G.bifoldLeft(gab, c)(f, g)
    )

  override def bifoldRight[A, B, C](fab: F[G[A, B], G[A, B]],
                                    c: Eval[C]
  )(f: (A, Eval[C]) => Eval[C], g: (B, Eval[C]) => Eval[C]): Eval[C] =
    F.bifoldRight(fab, c)(
      (gab: G[A, B], c: Eval[C]) => G.bifoldRight(gab, c)(f, g),
      (gab: G[A, B], c: Eval[C]) => G.bifoldRight(gab, c)(f, g)
    )
}
