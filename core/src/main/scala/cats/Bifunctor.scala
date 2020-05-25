package cats
import simulacrum.typeclass
import scala.annotation.implicitNotFound

/**
 * A type class of types which give rise to two independent, covariant
 * functors.
 */
@implicitNotFound("Could not find an instance of Bifunctor for ${F}")
@typeclass trait Bifunctor[F[_, _]] extends Serializable { self =>

  /**
   * The quintessential method of the Bifunctor trait, it applies a
   * function to each "side" of the bifunctor.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   *
   * scala> val x: (List[String], Int) = (List("foo", "bar"), 3)
   * scala> x.bimap(_.headOption, _.toLong + 1)
   * res0: (Option[String], Long) = (Some(foo),4)
   * }}}
   */
  def bimap[A, B, C, D](fab: F[A, B])(f: A => C, g: B => D): F[C, D]

  def rightFunctor[X]: Functor[F[X, *]] =
    new RightFunctor[F, X] { val F = self }

  def leftFunctor[X]: Functor[F[*, X]] =
    new LeftFunctor[F, X] { val F = self }

  // derived methods
  /**
   * apply a function to the "left" functor
   */
  def leftMap[A, B, C](fab: F[A, B])(f: A => C): F[C, B] = bimap(fab)(f, identity)

  /** The composition of two Bifunctors is itself a Bifunctor */
  def compose[G[_, _]](implicit G0: Bifunctor[G]): Bifunctor[λ[(α, β) => F[G[α, β], G[α, β]]]] =
    new ComposedBifunctor[F, G] {
      val F = self
      val G = G0
    }

  /**
   * Widens A into a supertype AA.
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> sealed trait Foo
   * scala> case object Bar extends Foo
   * scala> val x1: Either[Bar.type, Int] = Either.left(Bar)
   * scala> val x2: Either[Foo, Int] = x1.leftWiden
   * }}}
   */
  def leftWiden[A, B, AA >: A](fab: F[A, B]): F[AA, B] = fab.asInstanceOf[F[AA, B]]
}

object Bifunctor {
  implicit def catsBifunctorForEither: Bifunctor[Either] = cats.instances.either.catsStdBitraverseForEither
  implicit def catsBifunctorForTuple2: Bifunctor[Tuple2] = cats.instances.tuple.catsStdBitraverseForTuple2

  /****************************************************************************/
  /* THE FOLLOWING CODE IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!!      */
  /****************************************************************************/
  /**
   * Summon an instance of [[Bifunctor]] for `F`.
   */
  @inline def apply[F[_, _]](implicit instance: Bifunctor[F]): Bifunctor[F] = instance

  trait Ops[F[_, _], A, B] {
    type TypeClassType <: Bifunctor[F]
    def self: F[A, B]
    val typeClassInstance: TypeClassType
    def bimap[C, D](f: A => C, g: B => D): F[C, D] = typeClassInstance.bimap[A, B, C, D](self)(f, g)
    def leftMap[C](f: A => C): F[C, B] = typeClassInstance.leftMap[A, B, C](self)(f)
    def leftWiden[C >: A]: F[C, B] = typeClassInstance.leftWiden[A, B, C](self)
  }
  trait AllOps[F[_, _], A, B] extends Ops[F, A, B]
  trait ToBifunctorOps {
    implicit def toBifunctorOps[F[_, _], A, B](target: F[A, B])(implicit tc: Bifunctor[F]): Ops[F, A, B] {
      type TypeClassType = Bifunctor[F]
    } = new Ops[F, A, B] {
      type TypeClassType = Bifunctor[F]
      val self: F[A, B] = target
      val typeClassInstance: TypeClassType = tc
    }
  }
  object nonInheritedOps extends ToBifunctorOps
  object ops {
    implicit def toAllBifunctorOps[F[_, _], A, B](target: F[A, B])(implicit tc: Bifunctor[F]): AllOps[F, A, B] {
      type TypeClassType = Bifunctor[F]
    } = new AllOps[F, A, B] {
      type TypeClassType = Bifunctor[F]
      val self: F[A, B] = target
      val typeClassInstance: TypeClassType = tc
    }
  }

  /****************************************************************************/
  /* END OF SIMULACRUM-MANAGED CODE                                           */
  /****************************************************************************/

}

private[cats] trait ComposedBifunctor[F[_, _], G[_, _]] extends Bifunctor[λ[(A, B) => F[G[A, B], G[A, B]]]] {
  def F: Bifunctor[F]
  def G: Bifunctor[G]

  override def bimap[A, B, C, D](fab: F[G[A, B], G[A, B]])(f: A => C, g: B => D): F[G[C, D], G[C, D]] = {
    val innerBimap: G[A, B] => G[C, D] = gab => G.bimap(gab)(f, g)
    F.bimap(fab)(innerBimap, innerBimap)
  }
}

abstract private class LeftFunctor[F[_, _], X] extends Functor[F[*, X]] {
  implicit val F: Bifunctor[F]

  override def map[A, C](fax: F[A, X])(f: A => C): F[C, X] =
    F.bimap(fax)(f, identity)
}

abstract private class RightFunctor[F[_, _], X] extends Functor[F[X, *]] {
  implicit val F: Bifunctor[F]

  override def map[A, C](fxa: F[X, A])(f: A => C): F[X, C] =
    F.bimap(fxa)(identity, f)
}
