package cats
package syntax

trait TraverseSyntax1 {
  implicit def traverseSyntaxU[FA](fa: FA)(implicit U: Unapply[Traverse,FA]): TraverseOps[U.M, U.A] =
    new TraverseOps(U.subst(fa))(U.TC)
}

trait TraverseSyntax extends TraverseSyntax1 {
  // TODO: use simulacrum instances eventually
  implicit def traverseSyntax[F[_]: Traverse, A](fa: F[A]): TraverseOps[F, A] =
    new TraverseOps(fa)

  implicit def nestedTraverseSyntax[F[_]: Traverse, G[_], A](fga: F[G[A]]): NestedTraverseOps[F, G, A] =
    new NestedTraverseOps[F, G, A](fga)
}

final class TraverseOps[F[_], A](fa: F[A])(implicit F: Traverse[F]) {
  /**
   * @see [[Traverse.traverse]]
   *
   * Example:
   * {{{
   * scala> import cats.data.Xor
   * scala> import cats.std.list._
   * scala> import cats.std.option._
   * scala> import cats.syntax.traverse._
   * scala> def parseInt(s: String): Option[Int] = Xor.catchOnly[NumberFormatException](s.toInt).toOption
   * scala> List("1", "2", "3").traverse(parseInt)
   * res0: Option[List[Int]] = Some(List(1, 2, 3))
   * scala> List("1", "two", "3").traverse(parseInt)
   * res1: Option[List[Int]] = None
   * }}}
   */
  def traverse[G[_]: Applicative, B](f: A => G[B]): G[F[B]] =
    F.traverse(fa)(f)

  /**
   * @see [[Traverse.traverse]]
   *
   * Example:
   * {{{
   * scala> import cats.data.Xor
   * scala> import cats.std.list._
   * scala> import cats.syntax.traverse._
   * scala> def parseInt(s: String): Xor[String, Int] = Xor.catchOnly[NumberFormatException](s.toInt).leftMap(_ => "no number")
   * scala> val ns = List("1", "2", "3")
   * scala> ns.traverseU(parseInt)
   * res0: Xor[String, List[Int]] = Right(List(1, 2, 3))
   * scala> ns.traverse[Xor[String, ?], Int](parseInt)
   * res1: Xor[String, List[Int]] = Right(List(1, 2, 3))
   * }}}
   */
  def traverseU[GB](f: A => GB)(implicit U: Unapply[Applicative, GB]): U.M[F[U.A]] =
    F.traverseU[A, GB](fa)(f)(U)

  /**
   * @see [[Traverse.traverseM]]
   *
   * Example:
   * {{{
   * scala> import cats.data.Xor
   * scala> import cats.std.list._
   * scala> import cats.std.option._
   * scala> import cats.syntax.traverse._
   * scala> def parseInt(s: String): Option[Int] = Xor.catchOnly[NumberFormatException](s.toInt).toOption
   * scala> val x = Option(List("1", "two", "3"))
   * scala> x.traverseM(_.map(parseInt))
   * res0: List[Option[Int]] = List(Some(1), None, Some(3))
   * }}}
   */
  def traverseM[G[_]: Applicative, B](f: A => G[F[B]])(implicit F2: FlatMap[F]): G[F[B]] =
    F.traverseM(fa)(f)

  /**
   * @see [[Traverse.sequence]]
   *
   * Example:
   * {{{
   * scala> import cats.std.list._
   * scala> import cats.std.option._
   * scala> import cats.syntax.traverse._
   * scala> val x: List[Option[Int]] = List(Some(1), Some(2))
   * scala> val y: List[Option[Int]] = List(None, Some(2))
   * scala> x.sequence
   * res0: Option[List[Int]] = Some(List(1, 2))
   * scala> y.sequence
   * res1: Option[List[Int]] = None
   * }}}
   */
   def sequence[G[_], B](implicit G: Applicative[G], ev: A =:= G[B]): G[F[B]] =
    F.sequence(fa.asInstanceOf[F[G[B]]])

  /**
   * @see [[Traverse.sequenceU]]
   *
   * Example:
   * {{{
   * scala> import cats.data.{Validated, ValidatedNel}
   * scala> import cats.std.list._
   * scala> import cats.syntax.traverse._
   * scala> val x: List[ValidatedNel[String, Int]] = List(Validated.valid(1), Validated.invalid("a"), Validated.invalid("b")).map(_.toValidatedNel)
   * scala> x.sequenceU
   * res0: cats.data.ValidatedNel[String,List[Int]] = Invalid(OneAnd(a,List(b)))
   * scala> x.sequence[ValidatedNel[String, ?], Int]
   * res1: cats.data.ValidatedNel[String,List[Int]] = Invalid(OneAnd(a,List(b)))
   * }}}
   */
  def sequenceU(implicit U: Unapply[Applicative,A]): U.M[F[U.A]] =
    F.sequenceU[A](fa)(U)
}

final class NestedTraverseOps[F[_], G[_], A](fga: F[G[A]])(implicit F: Traverse[F]) {
  def sequence(implicit G: Applicative[G]): G[F[A]] = F.sequence(fga)
}
