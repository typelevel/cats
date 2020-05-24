package cats

import simulacrum.typeclass
import scala.annotation.implicitNotFound

@implicitNotFound("Could not find an instance of Alternative for ${F}")
@typeclass trait Alternative[F[_]] extends Applicative[F] with MonoidK[F] { self =>

  /**
   * Fold over the inner structure to combine all of the values with
   * our combine method inherited from MonoidK. The result is for us
   * to accumulate all of the "interesting" values of the inner G, so
   * if G is Option, we collect all the Some values, if G is Either,
   * we collect all the Right values, etc.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> val x: List[Vector[Int]] = List(Vector(1, 2), Vector(3, 4))
   * scala> Alternative[List].unite(x)
   * res0: List[Int] = List(1, 2, 3, 4)
   * }}}
   */
  def unite[G[_], A](fga: F[G[A]])(implicit FM: Monad[F], G: Foldable[G]): F[A] =
    FM.flatMap(fga) { ga =>
      G.foldLeft(ga, empty[A])((acc, a) => combineK(acc, pure(a)))
    }

  /**
   * Separate the inner foldable values into the "lefts" and "rights"
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> val l: List[Either[String, Int]] = List(Right(1), Left("error"))
   * scala> Alternative[List].separate(l)
   * res0: (List[String], List[Int]) = (List(error),List(1))
   * }}}
   */
  def separate[G[_, _], A, B](fgab: F[G[A, B]])(implicit FM: Monad[F], G: Bifoldable[G]): (F[A], F[B]) = {
    val as = FM.flatMap(fgab)(gab => G.bifoldMap(gab)(pure, _ => empty[A])(algebra[A]))
    val bs = FM.flatMap(fgab)(gab => G.bifoldMap(gab)(_ => empty[B], pure)(algebra[B]))
    (as, bs)
  }

  /**
   * Separate the inner foldable values into the "lefts" and "rights".
   * A variant of [[separate]] that is specialized
   * for Fs that have Foldable instances
   * which allows for a single-pass implementation
   * (as opposed to {{{separate}}} which is 2-pass).
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> val l: List[Either[String, Int]] = List(Right(1), Left("error"))
   * scala> Alternative[List].separateFoldable(l)
   * res0: (List[String], List[Int]) = (List(error),List(1))
   * }}}
   */
  def separateFoldable[G[_, _], A, B](fgab: F[G[A, B]])(implicit G: Bifoldable[G], FF: Foldable[F]): (F[A], F[B]) =
    FF.foldLeft(fgab, (empty[A], empty[B])) {
      case (mamb, gab) =>
        G.bifoldLeft(gab, mamb)(
          (t, a) => (combineK(t._1, pure(a)), t._2),
          (t, b) => (t._1, combineK(t._2, pure(b)))
        )
    }

  /**
   * Return ().pure[F] if `condition` is true, `empty` otherwise
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> def even(i: Int): Option[String] = Alternative[Option].guard(i % 2 == 0).as("even")
   * scala> even(2)
   * res0: Option[String] = Some(even)
   * scala> even(3)
   * res1: Option[String] = None
   * }}}
   */
  def guard(condition: Boolean): F[Unit] =
    if (condition) unit else empty

  override def compose[G[_]: Applicative]: Alternative[λ[α => F[G[α]]]] =
    new ComposedAlternative[F, G] {
      val F = self
      val G = Applicative[G]
    }
}

object Alternative {

  /****************************************************************************/
  /* THE FOLLOWING CODE IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!!      */
  /****************************************************************************/
  /**
   * Summon an instance of [[Alternative]] for `F`.
   */
  @inline def apply[F[_]](implicit instance: Alternative[F]): Alternative[F] = instance

  trait Ops[F[_], A] {
    type TypeClassType <: Alternative[F]
    def self: F[A]
    val typeClassInstance: TypeClassType
    def unite[G[_], B](implicit ev$1: A <:< G[B], FM: Monad[F], G: Foldable[G]): F[B] =
      typeClassInstance.unite[G, B](self.asInstanceOf[F[G[B]]])(FM, G)
    def separate[G[_, _], B, C](implicit ev$1: A <:< G[B, C], FM: Monad[F], G: Bifoldable[G]): (F[B], F[C]) =
      typeClassInstance.separate[G, B, C](self.asInstanceOf[F[G[B, C]]])(FM, G)
    def separateFoldable[G[_, _], B, C](implicit ev$1: A <:< G[B, C], G: Bifoldable[G], FF: Foldable[F]): (F[B], F[C]) =
      typeClassInstance.separateFoldable[G, B, C](self.asInstanceOf[F[G[B, C]]])(G, FF)
  }
  trait AllOps[F[_], A] extends Ops[F, A] with Applicative.AllOps[F, A] with MonoidK.AllOps[F, A] {
    type TypeClassType <: Alternative[F]
  }
  trait ToAlternativeOps {
    implicit def toAlternativeOps[F[_], A](target: F[A])(implicit tc: Alternative[F]): Ops[F, A] {
      type TypeClassType = Alternative[F]
    } = new Ops[F, A] {
      type TypeClassType = Alternative[F]
      val self: F[A] = target
      val typeClassInstance: TypeClassType = tc
    }
  }
  object nonInheritedOps extends ToAlternativeOps
  object ops {
    implicit def toAllAlternativeOps[F[_], A](target: F[A])(implicit tc: Alternative[F]): AllOps[F, A] {
      type TypeClassType = Alternative[F]
    } = new AllOps[F, A] {
      type TypeClassType = Alternative[F]
      val self: F[A] = target
      val typeClassInstance: TypeClassType = tc
    }
  }

  /****************************************************************************/
  /* END OF SIMULACRUM-MANAGED CODE                                           */
  /****************************************************************************/

}
