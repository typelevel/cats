package cats
package data

import instances.option.{catsStdInstancesForOption => optionInstance}

/**
 * `OptionT[F[_], A]` is a light wrapper on an `F[Option[A]]` with some
 * convenient methods for working with this nested structure.
 *
 * It may also be said that `OptionT` is a monad transformer for `Option`.
 *
 * For more information, see the [[http://typelevel.org/cats/tut/optiont.html documentation]].
 */
final case class OptionT[F[_], A](value: F[Option[A]]) {

  def fold[B](default: => B)(f: A => B)(implicit F: Functor[F]): F[B] =
    F.map(value)(_.fold(default)(f))

  /**
   * Catamorphism on the Option. This is identical to [[fold]], but it only has
   * one parameter list, which can result in better type inference in some
   * contexts.
   */
  def cata[B](default: => B, f: A => B)(implicit F: Functor[F]): F[B] =
    fold(default)(f)

  def map[B](f: A => B)(implicit F: Functor[F]): OptionT[F, B] =
    OptionT(F.map(value)(_.map(f)))

  def semiflatMap[B](f: A => F[B])(implicit F: Monad[F]): OptionT[F, B] =
    flatMap(a => OptionT.liftF(f(a)))

  def mapFilter[B](f: A => Option[B])(implicit F: Functor[F]): OptionT[F, B] =
    OptionT(F.map(value)(_.flatMap(f)))

  def flatMap[B](f: A => OptionT[F, B])(implicit F: Monad[F]): OptionT[F, B] =
    flatMapF(a => f(a).value)

  def flatMapF[B](f: A => F[Option[B]])(implicit F: Monad[F]): OptionT[F, B] =
    OptionT(F.flatMap(value)(_.fold(F.pure[Option[B]](None))(f)))

  def transform[B](f: Option[A] => Option[B])(implicit F: Functor[F]): OptionT[F, B] =
    OptionT(F.map(value)(f))

  def subflatMap[B](f: A => Option[B])(implicit F: Functor[F]): OptionT[F, B] =
    transform(_.flatMap(f))

  def getOrElse(default: => A)(implicit F: Functor[F]): F[A] =
    F.map(value)(_.getOrElse(default))

  def getOrElseF(default: => F[A])(implicit F: Monad[F]): F[A] =
    F.flatMap(value)(_.fold(default)(F.pure))

  def collect[B](f: PartialFunction[A, B])(implicit F: Functor[F]): OptionT[F, B] =
    OptionT(F.map(value)(_.collect(f)))

  def exists(f: A => Boolean)(implicit F: Functor[F]): F[Boolean] =
    F.map(value)(_.exists(f))

  def filter(p: A => Boolean)(implicit F: Functor[F]): OptionT[F, A] =
    OptionT(F.map(value)(_.filter(p)))

  def withFilter(p: A => Boolean)(implicit F: Functor[F]): OptionT[F, A] =
    filter(p)(F)

  def filterNot(p: A => Boolean)(implicit F: Functor[F]): OptionT[F, A] =
    OptionT(F.map(value)(_.filterNot(p)))

  def forall(f: A => Boolean)(implicit F: Functor[F]): F[Boolean] =
    F.map(value)(_.forall(f))

  def isDefined(implicit F: Functor[F]): F[Boolean] =
    F.map(value)(_.isDefined)

  def isEmpty(implicit F: Functor[F]): F[Boolean] =
    F.map(value)(_.isEmpty)

  def orElse(default: => OptionT[F, A])(implicit F: Monad[F]): OptionT[F, A] =
    orElseF(default.value)

  def orElseF(default: => F[Option[A]])(implicit F: Monad[F]): OptionT[F, A] =
    OptionT(
      F.flatMap(value) {
        case s @ Some(_) => F.pure(s)
        case None => default
      })

  def toRight[L](left: => L)(implicit F: Functor[F]): XorT[F, L, A] =
    XorT(cata(Xor.Left(left), Xor.Right.apply))

  def toLeft[R](right: => R)(implicit F: Functor[F]): XorT[F, A, R] =
    XorT(cata(Xor.Right(right), Xor.Left.apply))

  def show(implicit F: Show[F[Option[A]]]): String = F.show(value)

  def compare(that: OptionT[F, A])(implicit o: Order[F[Option[A]]]): Int =
    o.compare(value, that.value)

  def partialCompare(that: OptionT[F, A])(implicit p: PartialOrder[F[Option[A]]]): Double =
    p.partialCompare(value, that.value)

  def ===(that: OptionT[F, A])(implicit eq: Eq[F[Option[A]]]): Boolean =
    eq.eqv(value, that.value)

  def traverseFilter[G[_], B](f: A => G[Option[B]])(implicit F: Traverse[F], G: Applicative[G]): G[OptionT[F, B]] =
    G.map(F.composeFilter(optionInstance).traverseFilter(value)(f))(OptionT.apply)

  def traverse[G[_], B](f: A => G[B])(implicit F: Traverse[F], G: Applicative[G]): G[OptionT[F, B]] =
    G.map(F.compose(optionInstance).traverse(value)(f))(OptionT.apply)

  def foldLeft[B](b: B)(f: (B, A) => B)(implicit F: Foldable[F]): B =
    F.compose(optionInstance).foldLeft(value, b)(f)

  def foldRight[B](lb: Eval[B])(f: (A, Eval[B]) => Eval[B])(implicit F: Foldable[F]): Eval[B] =
    F.compose(optionInstance).foldRight(value, lb)(f)

  /**
   * Transform this `OptionT[F, A]` into a `[[Nested]][F, Option, A]`.
   *
   * An example where `toNested` can be used, is to get the `Apply.ap` function with the
   * behavior from the composed `Apply` instances from `F` and `Option`, which is
   * inconsistent with the behavior of the `ap` from `Monad` of `OptionT`.
   *
   * {{{
   * scala> import cats.implicits._
   * scala> import cats.data.OptionT
   * scala> val ff: OptionT[List, Int => String] =
   *      |   OptionT(List(Option(_.toString), None))
   * scala> val fa: OptionT[List, Int] = OptionT(List(Option(1), Option(2)))
   * scala> ff.ap(fa)
   * res0: OptionT[List,String] = OptionT(List(Some(1), Some(2), None))
   * scala> OptionT(ff.toNested.ap(fa.toNested).value)
   * res1: OptionT[List,String] = OptionT(List(Some(1), Some(2), None, None))
   * }}}
   */
  def toNested: Nested[F, Option, A] = Nested(value)
}

object OptionT extends OptionTInstances {
  def pure[F[_], A](a: A)(implicit F: Applicative[F]): OptionT[F, A] =
    OptionT(F.pure(Some(a)))

  /** An alias for pure */
  def some[F[_], A](a: A)(implicit F: Applicative[F]): OptionT[F, A] =
    pure(a)

  def none[F[_], A](implicit F: Applicative[F]) : OptionT[F, A] =
    OptionT(F.pure(None))

  /**
   * Transforms an `Option` into an `OptionT`, lifted into the specified `Applicative`.
   *
   * Note: The return type is a `FromOptionPartiallyApplied[F]`, which has an apply method
   * on it, allowing you to call `fromOption` like this:
   * {{{
   * scala> import cats.implicits._
   * scala> val o: Option[Int] = Some(2)
   * scala> OptionT.fromOption[List](o)
   * res0: OptionT[List, Int] = OptionT(List(Some(2)))
   * }}}
   *
   * The reason for the indirection is to emulate currying type parameters.
   */
  def fromOption[F[_]]: FromOptionPartiallyApplied[F] = new FromOptionPartiallyApplied

  final class FromOptionPartiallyApplied[F[_]] private[OptionT] {
    def apply[A](value: Option[A])(implicit F: Applicative[F]): OptionT[F, A] =
      OptionT(F.pure(value))
  }

  /**
   * Lifts the `F[A]` Functor into an `OptionT[F, A]`.
   */
  def liftF[F[_], A](fa: F[A])(implicit F: Functor[F]): OptionT[F, A] = OptionT(F.map(fa)(Some(_)))
}

private[data] sealed trait OptionTInstances extends OptionTInstances0 {
  implicit def catsDataMonadRecForOptionT[F[_]](implicit F0: MonadRec[F]): MonadRec[OptionT[F, ?]] =
    new OptionTMonadRec[F] { implicit val F = F0 }

  implicit def catsDataFoldableForOptionT[F[_]](implicit F0: Foldable[F]): Foldable[OptionT[F, ?]] =
    new OptionTFoldable[F] { implicit val F = F0 }

  implicit def catsDataSemigroupForOptionT[F[_], A](implicit F0: Semigroup[F[Option[A]]]): Semigroup[OptionT[F, A]] =
    new OptionTSemigroup[F, A] { implicit val F = F0 }

  implicit def catsDataOrderForOptionT[F[_], A](implicit F0: Order[F[Option[A]]]): Order[OptionT[F, A]] =
    new OptionTOrder[F, A] { implicit val F = F0 }

  implicit def catsDataShowForOptionT[F[_], A](implicit F: Show[F[Option[A]]]): Show[OptionT[F, A]] =
    functor.Contravariant[Show].contramap(F)(_.value)
}

private[data] sealed trait OptionTInstances0 extends OptionTInstances1 {
  implicit def catsDataMonadErrorForOptionT[F[_], E](implicit F0: MonadError[F, E]): MonadError[OptionT[F, ?], E] =
    new OptionTMonadError[F, E] { implicit val F = F0 }

  implicit def catsDataSemigroupKForOptionT[F[_]](implicit F0: Monad[F]): SemigroupK[OptionT[F, ?]] =
    new OptionTSemigroupK[F] { implicit val F = F0 }

  implicit def catsDataMonoidForOptionT[F[_], A](implicit F0: Monoid[F[Option[A]]]): Monoid[OptionT[F, A]] =
    new OptionTMonoid[F, A] { implicit val F = F0 }

  implicit def catsDataPartialOrderForOptionT[F[_], A](implicit F0: PartialOrder[F[Option[A]]]): PartialOrder[OptionT[F, A]] =
    new OptionTPartialOrder[F, A] { implicit val F = F0 }
}

private[data] sealed trait OptionTInstances1 extends OptionTInstances2 {
  implicit def catsDataMonadForOptionT[F[_]](implicit F0: Monad[F]): Monad[OptionT[F, ?]] =
    new OptionTMonad[F] { implicit val F = F0 }

  // do NOT change this to val! I know it looks like it should work, and really I agree, but it doesn't (for... reasons)
  implicit def catsDataTransLiftForOptionT: TransLift.Aux[OptionT, Functor] =
    new TransLift[OptionT] {
      type TC[M[_]] = Functor[M]

      def liftT[M[_]: Functor, A](ma: M[A]): OptionT[M, A] = OptionT.liftF(ma)
    }

  implicit def catsDataMonoidKForOptionT[F[_]](implicit F0: Monad[F]): MonoidK[OptionT[F, ?]] =
    new OptionTMonoidK[F] { implicit val F = F0 }

  implicit def catsDataEqForOptionT[F[_], A](implicit F0: Eq[F[Option[A]]]): Eq[OptionT[F, A]] =
    new OptionTEq[F, A] { implicit val F = F0 }
}

private[data] sealed trait OptionTInstances2 extends OptionTInstances3 {
  implicit def catsDataTraverseForOptionT[F[_]](implicit F0: Traverse[F]): TraverseFilter[OptionT[F, ?]] =
    new OptionTTraverseFilter[F] { implicit val F = F0 }
}

private[data] sealed trait OptionTInstances3 {
  implicit def catsDataFunctorFilterForOptionT[F[_]](implicit F0: Functor[F]): FunctorFilter[OptionT[F, ?]] =
    new OptionTFunctor[F] { implicit val F = F0 }
}

private[data] trait OptionTFunctor[F[_]] extends FunctorFilter[OptionT[F, ?]] {
  implicit def F: Functor[F]

  override def map[A, B](fa: OptionT[F, A])(f: A => B): OptionT[F, B] = fa.map(f)

  override def mapFilter[A, B](fa: OptionT[F, A])(f: A => Option[B]): OptionT[F, B] =
    fa.mapFilter(f)
}

private[data] trait OptionTMonad[F[_]] extends Monad[OptionT[F, ?]] {
  implicit def F: Monad[F]

  def pure[A](a: A): OptionT[F, A] = OptionT.pure(a)

  def flatMap[A, B](fa: OptionT[F, A])(f: A => OptionT[F, B]): OptionT[F, B] = fa.flatMap(f)

  override def map[A, B](fa: OptionT[F, A])(f: A => B): OptionT[F, B] = fa.map(f)
}

private[data] trait OptionTMonadRec[F[_]] extends MonadRec[OptionT[F, ?]] with OptionTMonad[F] {
  implicit def F: MonadRec[F]

  def tailRecM[A, B](a: A)(f: A => OptionT[F, A Xor B]): OptionT[F, B] =
    OptionT(F.tailRecM(a)(a0 => F.map(f(a0).value)(
      _.fold(Xor.right[A, Option[B]](None))(_.map(Some(_)))
    )))
}

private trait OptionTMonadError[F[_], E] extends MonadError[OptionT[F, ?], E] with OptionTMonad[F] {
  override def F: MonadError[F, E]

  override def raiseError[A](e: E): OptionT[F, A] =
    OptionT(F.map(F.raiseError[A](e))(Some(_)))

  override def handleErrorWith[A](fa: OptionT[F, A])(f: E => OptionT[F, A]): OptionT[F, A] =
    OptionT(F.handleErrorWith(fa.value)(f(_).value))
}

private[data] trait OptionTFoldable[F[_]] extends Foldable[OptionT[F, ?]] {
  implicit def F: Foldable[F]

  def foldLeft[A, B](fa: OptionT[F, A], b: B)(f: (B, A) => B): B =
    fa.foldLeft(b)(f)

  def foldRight[A, B](fa: OptionT[F, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    fa.foldRight(lb)(f)
}

private[data] sealed trait OptionTTraverseFilter[F[_]] extends TraverseFilter[OptionT[F, ?]] with OptionTFoldable[F] {
  implicit def F: Traverse[F]

  def traverseFilter[G[_]: Applicative, A, B](fa: OptionT[F, A])(f: A => G[Option[B]]): G[OptionT[F, B]] =
    fa traverseFilter f

  override def traverse[G[_]: Applicative, A, B](fa: OptionT[F, A])(f: A => G[B]): G[OptionT[F, B]] =
    fa traverse f
}

private[data] trait OptionTSemigroup[F[_], A] extends Semigroup[OptionT[F, A]] {
  implicit val F: Semigroup[F[Option[A]]]

  def combine(x: OptionT[F, A], y: OptionT[F, A]): OptionT[F, A] =
    OptionT(F.combine(x.value, y.value))
}

private[data] trait OptionTMonoid[F[_], A] extends Monoid[OptionT[F, A]] with OptionTSemigroup[F, A] {
  implicit val F: Monoid[F[Option[A]]]

  def empty: OptionT[F, A] = OptionT(F.empty)
}

private[data] trait OptionTSemigroupK[F[_]] extends SemigroupK[OptionT[F, ?]] {
  implicit def F: Monad[F]

  def combineK[A](x: OptionT[F, A], y: OptionT[F, A]): OptionT[F, A] = x orElse y
}

private[data] trait OptionTMonoidK[F[_]] extends MonoidK[OptionT[F, ?]] with OptionTSemigroupK[F] {
  def empty[A]: OptionT[F, A] = OptionT.none[F, A]
}

private[data] sealed trait OptionTEq[F[_], A] extends Eq[OptionT[F, A]] {
  implicit def F: Eq[F[Option[A]]]

  override def eqv(x: OptionT[F, A], y: OptionT[F, A]): Boolean = x === y
}

private[data] sealed trait OptionTPartialOrder[F[_], A] extends PartialOrder[OptionT[F, A]] with OptionTEq[F, A]{
  override implicit def F: PartialOrder[F[Option[A]]]

  override def partialCompare(x: OptionT[F, A], y: OptionT[F, A]): Double = x partialCompare y
}

private[data] sealed trait OptionTOrder[F[_], A] extends Order[OptionT[F, A]] with OptionTPartialOrder[F, A]{
  override implicit def F: Order[F[Option[A]]]

  override def compare(x: OptionT[F, A], y: OptionT[F, A]): Int = x compare y
}
