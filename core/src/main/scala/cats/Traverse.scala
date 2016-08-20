package cats

import simulacrum.typeclass

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
@typeclass trait Traverse[F[_]] extends Functor[F] with Foldable[F] { self =>

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
   * Behaves just like traverse, but uses [[Unapply]] to find the
   * Applicative instance for G.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> def parseInt(s: String): Either[String, Int] = Either.catchOnly[NumberFormatException](s.toInt).leftMap(_ => "no number")
   * scala> val ns = List("1", "2", "3")
   * scala> ns.traverseU(parseInt)
   * res0: Either[String, List[Int]] = Right(List(1, 2, 3))
   * scala> ns.traverse[Either[String, ?], Int](parseInt)
   * res1: Either[String, List[Int]] = Right(List(1, 2, 3))
   * }}}
   */
  def traverseU[A, GB](fa: F[A])(f: A => GB)(implicit U: Unapply[Applicative, GB]): U.M[F[U.A]] =
    U.TC.traverse(fa)(a => U.subst(f(a)))(this)

  /**
   * A traverse followed by flattening the inner result.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> def parseInt(s: String): Option[Int] = Either.catchOnly[NumberFormatException](s.toInt).toOption
   * scala> val x = Option(List("1", "two", "3"))
   * scala> x.traverseM(_.map(parseInt))
   * res0: List[Option[Int]] = List(Some(1), None, Some(3))
   * }}}
   */
  def traverseM[G[_], A, B](fa: F[A])(f: A => G[F[B]])(implicit G: Applicative[G], F: FlatMap[F]): G[F[B]] =
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
   * Behaves just like sequence, but uses [[Unapply]] to find the
   * Applicative instance for G.
   *
   * Example:
   * {{{
   * scala> import cats.data.{Validated, ValidatedNel}
   * scala> import cats.implicits._
   * scala> val x: List[ValidatedNel[String, Int]] = List(Validated.valid(1), Validated.invalid("a"), Validated.invalid("b")).map(_.toValidatedNel)
   * scala> x.sequenceU
   * res0: cats.data.ValidatedNel[String,List[Int]] = Invalid(NonEmptyList(a, b))
   * scala> x.sequence[ValidatedNel[String, ?], Int]
   * res1: cats.data.ValidatedNel[String,List[Int]] = Invalid(NonEmptyList(a, b))
   * }}}
   */
  def sequenceU[GA](fga: F[GA])(implicit U: Unapply[Applicative, GA]): U.M[F[U.A]] =
    traverse(fga)(U.subst)(U.TC)

  def compose[G[_]: Traverse]: Traverse[λ[α => F[G[α]]]] =
    new ComposedTraverse[F, G] {
      val F = self
      val G = Traverse[G]
    }

  def composeFilter[G[_]: TraverseFilter]: TraverseFilter[λ[α => F[G[α]]]] =
    new ComposedTraverseFilter[F, G] {
      val F = self
      val G = TraverseFilter[G]
    }

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    traverse[Id, A, B](fa)(f)
}
