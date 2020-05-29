package cats

import cats.data.State
import cats.data.StateT

import simulacrum.typeclass
import scala.annotation.implicitNotFound

/**
 * Traverse, also known as Traversable.
 *
 * Traversal over a structure with an effect.
 *
 * Traversing with the [[cats.Id]] effect is equivalent to [[cats.Functor]]#map.
 * Traversing with the [[cats.data.Const]] effect where the first type parameter has
 * a [[cats.Monoid]] instance is equivalent to [[cats.Foldable]]#fold.
 *
 * See: [[https://www.cs.ox.ac.uk/jeremy.gibbons/publications/iterator.pdf The Essence of the Iterator Pattern]]
 */
@implicitNotFound("Could not find an instance of Traverse for ${F}")
@typeclass trait Traverse[F[_]] extends Functor[F] with Foldable[F] with UnorderedTraverse[F] { self =>

  /**
   * Given a function which returns a G effect, thread this effect
   * through the running of this function on all the values in F,
   * returning an F[B] in a G context.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> def parseInt(s: String): Option[Int] = Either.catchOnly[NumberFormatException](s.toInt).toOption
   * scala> List("1", "2", "3").traverse(parseInt)
   * res0: Option[List[Int]] = Some(List(1, 2, 3))
   * scala> List("1", "two", "3").traverse(parseInt)
   * res1: Option[List[Int]] = None
   * }}}
   */
  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

  /**
   * A traverse followed by flattening the inner result.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> def parseInt(s: String): Option[Int] = Either.catchOnly[NumberFormatException](s.toInt).toOption
   * scala> val x = Option(List("1", "two", "3"))
   * scala> x.flatTraverse(_.map(parseInt))
   * res0: List[Option[Int]] = List(Some(1), None, Some(3))
   * }}}
   */
  def flatTraverse[G[_], A, B](fa: F[A])(f: A => G[F[B]])(implicit G: Applicative[G], F: FlatMap[F]): G[F[B]] =
    G.map(traverse(fa)(f))(F.flatten)

  /**
   * Thread all the G effects through the F structure to invert the
   * structure from F[G[A]] to G[F[A]].
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> val x: List[Option[Int]] = List(Some(1), Some(2))
   * scala> val y: List[Option[Int]] = List(None, Some(2))
   * scala> x.sequence
   * res0: Option[List[Int]] = Some(List(1, 2))
   * scala> y.sequence
   * res1: Option[List[Int]] = None
   * }}}
   */
  def sequence[G[_]: Applicative, A](fga: F[G[A]]): G[F[A]] =
    traverse(fga)(ga => ga)

  /**
   * Thread all the G effects through the F structure and flatten to invert the
   * structure from F[G[F[A]]] to G[F[A]].
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> val x: List[Option[List[Int]]] = List(Some(List(1, 2)), Some(List(3)))
   * scala> val y: List[Option[List[Int]]] = List(None, Some(List(3)))
   * scala> x.flatSequence
   * res0: Option[List[Int]] = Some(List(1, 2, 3))
   * scala> y.flatSequence
   * res1: Option[List[Int]] = None
   * }}}
   */
  def flatSequence[G[_], A](fgfa: F[G[F[A]]])(implicit G: Applicative[G], F: FlatMap[F]): G[F[A]] =
    G.map(sequence(fgfa))(F.flatten)

  def compose[G[_]: Traverse]: Traverse[λ[α => F[G[α]]]] =
    new ComposedTraverse[F, G] {
      val F = self
      val G = Traverse[G]
    }

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    traverse[Id, A, B](fa)(f)

  /**
   * Akin to [[map]], but also provides the value's index in structure
   * F when calling the function.
   */
  def mapWithIndex[A, B](fa: F[A])(f: (A, Int) => B): F[B] =
    traverse(fa)(a => State((s: Int) => (s + 1, f(a, s)))).runA(0).value

  /**
   * Akin to [[traverse]], but also provides the value's index in
   * structure F when calling the function.
   *
   * This performs the traversal in a single pass but requires that
   * effect G is monadic. An applicative traversal can be performed in
   * two passes using [[zipWithIndex]] followed by [[traverse]].
   */
  def traverseWithIndexM[G[_], A, B](fa: F[A])(f: (A, Int) => G[B])(implicit G: Monad[G]): G[F[B]] =
    traverse(fa)(a => StateT((s: Int) => G.map(f(a, s))(b => (s + 1, b)))).runA(0)

  /**
   * Traverses through the structure F, pairing the values with
   * assigned indices.
   *
   * The behavior is consistent with the Scala collection library's
   * `zipWithIndex` for collections such as `List`.
   */
  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapWithIndex(fa)((a, i) => (a, i))

  override def unorderedTraverse[G[_]: CommutativeApplicative, A, B](sa: F[A])(f: (A) => G[B]): G[F[B]] =
    traverse(sa)(f)

  override def unorderedSequence[G[_]: CommutativeApplicative, A](fga: F[G[A]]): G[F[A]] =
    sequence(fga)
}

object Traverse {
  implicit def catsTraverseForEither[A]: Traverse[Either[A, *]] = cats.instances.either.catsStdInstancesForEither[A]

  /****************************************************************************/
  /* THE FOLLOWING CODE IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!!      */
  /****************************************************************************/

  /**
   * Summon an instance of [[Traverse]] for `F`.
   */
  @inline def apply[F[_]](implicit instance: Traverse[F]): Traverse[F] = instance

  trait Ops[F[_], A] extends Serializable {
    type TypeClassType <: Traverse[F]
    def self: F[A]
    val typeClassInstance: TypeClassType
    def traverse[G[_], B](f: A => G[B])(implicit ev$1: Applicative[G]): G[F[B]] =
      typeClassInstance.traverse[G, A, B](self)(f)
    def flatTraverse[G[_], B](f: A => G[F[B]])(implicit G: Applicative[G], F: FlatMap[F]): G[F[B]] =
      typeClassInstance.flatTraverse[G, A, B](self)(f)(G, F)
    def sequence[G[_], B](implicit ev$1: A <:< G[B], ev$2: Applicative[G]): G[F[B]] =
      typeClassInstance.sequence[G, B](self.asInstanceOf[F[G[B]]])
    def flatSequence[G[_], B](implicit ev$1: A <:< G[F[B]], G: Applicative[G], F: FlatMap[F]): G[F[B]] =
      typeClassInstance.flatSequence[G, B](self.asInstanceOf[F[G[F[B]]]])(G, F)
    def mapWithIndex[B](f: (A, Int) => B): F[B] = typeClassInstance.mapWithIndex[A, B](self)(f)
    def traverseWithIndexM[G[_], B](f: (A, Int) => G[B])(implicit G: Monad[G]): G[F[B]] =
      typeClassInstance.traverseWithIndexM[G, A, B](self)(f)(G)
    def zipWithIndex: F[(A, Int)] = typeClassInstance.zipWithIndex[A](self)
  }
  trait AllOps[F[_], A]
      extends Ops[F, A]
      with Functor.AllOps[F, A]
      with Foldable.AllOps[F, A]
      with UnorderedTraverse.AllOps[F, A] {
    type TypeClassType <: Traverse[F]
  }
  trait ToTraverseOps extends Serializable {
    implicit def toTraverseOps[F[_], A](target: F[A])(implicit tc: Traverse[F]): Ops[F, A] {
      type TypeClassType = Traverse[F]
    } =
      new Ops[F, A] {
        type TypeClassType = Traverse[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  object nonInheritedOps extends ToTraverseOps
  object ops {
    implicit def toAllTraverseOps[F[_], A](target: F[A])(implicit tc: Traverse[F]): AllOps[F, A] {
      type TypeClassType = Traverse[F]
    } =
      new AllOps[F, A] {
        type TypeClassType = Traverse[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }

  /****************************************************************************/
  /* END OF SIMULACRUM-MANAGED CODE                                           */
  /****************************************************************************/

}
