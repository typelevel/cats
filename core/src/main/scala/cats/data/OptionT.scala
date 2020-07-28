package cats
package data

/**
 * `OptionT[F[_], A]` is a light wrapper on an `F[Option[A]]` with some
 * convenient methods for working with this nested structure.
 *
 * It may also be said that `OptionT` is a monad transformer for `Option`.
 *
 * For more information, see the [[http://typelevel.org/cats/datatypes/optiont.html documentation]].
 */
final case class OptionT[F[_], A](value: F[Option[A]]) {

  def fold[B](default: => B)(f: A => B)(implicit F: Functor[F]): F[B] =
    F.map(value)(_.fold(default)(f))

  /**
   * Transform this `OptionT[F, A]` into a `F[C]`.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> import cats.data.OptionT
   *
   * scala> val optionT: OptionT[List, Int] = OptionT[List, Int](List(Some(23), None))
   * scala> optionT.foldF(Nil)(v => List(v, v * 2))
   * res0: List[Int] = List(23, 46)
   * }}}
   */
  def foldF[B](default: => F[B])(f: A => F[B])(implicit F: FlatMap[F]): F[B] =
    F.flatMap(value)(_.fold(default)(f))

  /**
   * Catamorphism on the Option. This is identical to [[fold]], but it only has
   * one parameter list, which can result in better type inference in some
   * contexts.
   */
  def cata[B](default: => B, f: A => B)(implicit F: Functor[F]): F[B] =
    fold(default)(f)

  /**
   * Effectful catamorphism on the Option. This is identical to [[foldF]], but it only has
   * one parameter list, which can result in better type inference in some
   * contexts.
   */
  def cataF[B](default: => F[B], f: A => F[B])(implicit F: FlatMap[F]): F[B] =
    foldF(default)(f)

  def map[B](f: A => B)(implicit F: Functor[F]): OptionT[F, B] =
    OptionT(F.map(value)(_.map(f)))

  def imap[B](f: A => B)(g: B => A)(implicit F: Invariant[F]): OptionT[F, B] =
    OptionT {
      F.imap(value)(_.map(f))(_.map(g))
    }

  def contramap[B](f: B => A)(implicit F: Contravariant[F]): OptionT[F, B] =
    OptionT {
      F.contramap(value)(_.map(f))
    }

  /**
   * Modify the context `F` using transformation `f`.
   */
  def mapK[G[_]](f: F ~> G): OptionT[G, A] = OptionT[G, A](f(value))

  def semiflatMap[B](f: A => F[B])(implicit F: Monad[F]): OptionT[F, B] =
    flatMap(a => OptionT.liftF(f(a)))

  def semiflatTap[B](f: A => F[B])(implicit F: Monad[F]): OptionT[F, A] =
    semiflatMap(a => F.as(f(a), a))

  def mapFilter[B](f: A => Option[B])(implicit F: Functor[F]): OptionT[F, B] =
    subflatMap(f)

  def flatMap[B](f: A => OptionT[F, B])(implicit F: Monad[F]): OptionT[F, B] =
    flatMapF(a => f(a).value)

  def flatMapF[B](f: A => F[Option[B]])(implicit F: Monad[F]): OptionT[F, B] =
    OptionT(F.flatMap(value)(_.fold(F.pure[Option[B]](None))(f)))

  def flatTransform[B](f: Option[A] => F[Option[B]])(implicit F: Monad[F]): OptionT[F, B] =
    OptionT(F.flatMap(value)(f))

  def transform[B](f: Option[A] => Option[B])(implicit F: Functor[F]): OptionT[F, B] =
    OptionT(F.map(value)(f))

  def subflatMap[B](f: A => Option[B])(implicit F: Functor[F]): OptionT[F, B] =
    transform(_.flatMap(f))

  /**
   * Perform an effect if the value inside the is a `None`, leaving the value untouched. Equivalent to [[orElseF]]
   * with an effect returning `None` as argument.
   */
  def flatTapNone[B](ifNone: => F[B])(implicit F: Monad[F]): OptionT[F, A] =
    OptionT(F.flatTap(value)(_.fold(F.void(ifNone))(_ => F.unit)))

  def getOrElse[B >: A](default: => B)(implicit F: Functor[F]): F[B] =
    F.map(value)(_.getOrElse(default))

  def getOrElseF[B >: A](default: => F[B])(implicit F: Monad[F]): F[B] =
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
    OptionT(F.flatMap(value) {
      case s @ Some(_) => F.pure(s)
      case None        => default
    })

  def toRight[L](left: => L)(implicit F: Functor[F]): EitherT[F, L, A] =
    EitherT(cata(Left(left), Right.apply))

  def toRightF[L](left: => F[L])(implicit F: Monad[F]): EitherT[F, L, A] =
    EitherT(cataF(F.map(left)(Left.apply[L, A]), a => F.pure(Right(a))))

  def toLeft[R](right: => R)(implicit F: Functor[F]): EitherT[F, A, R] =
    EitherT(cata(Right(right), Left.apply))

  def toLeftF[R](right: => F[R])(implicit F: Monad[F]): EitherT[F, A, R] =
    EitherT(cataF(F.map(right)(Right.apply[A, R]), a => F.pure(Left(a))))

  def show(implicit F: Show[F[Option[A]]]): String = F.show(value)

  def compare(that: OptionT[F, A])(implicit o: Order[F[Option[A]]]): Int =
    o.compare(value, that.value)

  def partialCompare(that: OptionT[F, A])(implicit p: PartialOrder[F[Option[A]]]): Double =
    p.partialCompare(value, that.value)

  def ===(that: OptionT[F, A])(implicit eq: Eq[F[Option[A]]]): Boolean =
    eq.eqv(value, that.value)

  def traverse[G[_], B](f: A => G[B])(implicit F: Traverse[F], G: Applicative[G]): G[OptionT[F, B]] =
    G.map(F.compose(Traverse[Option]).traverse(value)(f))(OptionT.apply)

  def foldLeft[B](b: B)(f: (B, A) => B)(implicit F: Foldable[F]): B =
    F.compose(Foldable[Option]).foldLeft(value, b)(f)

  def foldRight[B](lb: Eval[B])(f: (A, Eval[B]) => Eval[B])(implicit F: Foldable[F]): Eval[B] =
    F.compose(Foldable[Option]).foldRight(value, lb)(f)

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

  /**
   * Uses the [[http://typelevel.org/cats/guidelines.html#partially-applied-type-params Partially Applied Type Params technique]] for ergonomics.
   */
  final private[data] class PurePartiallyApplied[F[_]](private val dummy: Boolean = true) extends AnyVal {
    def apply[A](value: A)(implicit F: Applicative[F]): OptionT[F, A] =
      OptionT(F.pure(Some(value)))
  }

  /**
   * Creates a `OptionT[A]` from an `A`
   *
   * {{{
   * scala> import cats.implicits._
   * scala> OptionT.pure[List](2)
   * res0: OptionT[List, Int] = OptionT(List(Some(2)))
   * }}}
   */
  def pure[F[_]]: PurePartiallyApplied[F] = new PurePartiallyApplied[F]

  /**
   * An alias for pure
   *
   * {{{
   * scala> import cats.implicits._
   * scala> OptionT.some[List](2)
   * res0: OptionT[List, Int] = OptionT(List(Some(2)))
   * }}}
   */
  def some[F[_]]: PurePartiallyApplied[F] = pure

  def none[F[_], A](implicit F: Applicative[F]): OptionT[F, A] =
    OptionT(F.pure(None))

  /**
   * Transforms an `Option` into an `OptionT`, lifted into the specified `Applicative`.
   *
   * {{{
   * scala> import cats.implicits._
   * scala> val o: Option[Int] = Some(2)
   * scala> OptionT.fromOption[List](o)
   * res0: OptionT[List, Int] = OptionT(List(Some(2)))
   * }}}
   */
  def fromOption[F[_]]: FromOptionPartiallyApplied[F] = new FromOptionPartiallyApplied

  /**
   * Uses the [[http://typelevel.org/cats/guidelines.html#partially-applied-type-params Partially Applied Type Params technique]] for ergonomics.
   */
  final private[data] class FromOptionPartiallyApplied[F[_]](private val dummy: Boolean = true) extends AnyVal {
    def apply[A](value: Option[A])(implicit F: Applicative[F]): OptionT[F, A] =
      OptionT(F.pure(value))
  }

  /**
   * Lifts the `F[A]` Functor into an `OptionT[F, A]`.
   */
  def liftF[F[_], A](fa: F[A])(implicit F: Functor[F]): OptionT[F, A] = OptionT(F.map(fa)(Some(_)))

  /**
   * Same as [[liftF]], but expressed as a FunctionK for use with mapK
   * {{{
   * scala> import cats._, data._,  implicits._
   * scala> val a: EitherT[Eval, String, Int] = 1.pure[EitherT[Eval, String, *]]
   * scala> val b: EitherT[OptionT[Eval, *], String, Int] = a.mapK(OptionT.liftK)
   * scala> b.value.value.value
   * res0: Option[Either[String,Int]] = Some(Right(1))
   * }}}
   */
  def liftK[F[_]](implicit F: Functor[F]): F ~> OptionT[F, *] =
    new (F ~> OptionT[F, *]) { def apply[A](a: F[A]): OptionT[F, A] = OptionT.liftF(a) }

  /**
   * Creates a non-empty `OptionT[F, A]` from an `A` value if the given condition is `true`.
   * Otherwise, `none[F, A]` is returned. Analogous to `Option.when`.
   */
  def when[F[_], A](cond: Boolean)(a: => A)(implicit F: Applicative[F]): OptionT[F, A] =
    if (cond) OptionT.some[F](a) else OptionT.none[F, A]

  /**
   * Creates a non-empty `OptionT[F, A]` from an `F[A]` value if the given condition is `true`.
   * Otherwise, `none[F, A]` is returned. Analogous to `Option.when`.
   */
  def whenF[F[_], A](cond: Boolean)(fa: => F[A])(implicit F: Applicative[F]): OptionT[F, A] =
    if (cond) OptionT.liftF(fa) else OptionT.none[F, A]

  /**
   * Same as `whenF`, but expressed as a FunctionK for use with mapK.
   */
  def whenK[F[_]](cond: Boolean)(implicit F: Applicative[F]): F ~> OptionT[F, *] =
    new (F ~> OptionT[F, *]) { def apply[A](a: F[A]): OptionT[F, A] = OptionT.whenF(cond)(a) }

  /**
   * Creates a non-empty `OptionT[F, A]` from an `A` if the given condition is `false`.
   * Otherwise, `none[F, A]` is returned. Analogous to `Option.unless`.
   */
  def unless[F[_], A](cond: Boolean)(a: => A)(implicit F: Applicative[F]): OptionT[F, A] =
    OptionT.when(!cond)(a)

  /**
   * Creates an non-empty `OptionT[F, A]` from an `F[A]` if the given condition is `false`.
   * Otherwise, `none[F, A]` is returned. Analogous to `Option.unless`.
   */
  def unlessF[F[_], A](cond: Boolean)(fa: => F[A])(implicit F: Applicative[F]): OptionT[F, A] =
    OptionT.whenF(!cond)(fa)

  /**
   * Same as `unlessF`, but expressed as a FunctionK for use with mapK.
   */
  def unlessK[F[_]](cond: Boolean)(implicit F: Applicative[F]): F ~> OptionT[F, *] =
    new (F ~> OptionT[F, *]) { def apply[A](a: F[A]): OptionT[F, A] = OptionT.unlessF(cond)(a) }
}

sealed abstract private[data] class OptionTInstances extends OptionTInstances0 {
  // to maintain binary compatibility
  def catsDataMonadForOptionT[F[_]](implicit F0: Monad[F]): Monad[OptionT[F, *]] =
    new OptionTMonad[F] { implicit val F = F0 }

  implicit def catsDataTraverseForOptionT[F[_]](implicit F0: Traverse[F]): Traverse[OptionT[F, *]] =
    new OptionTTraverse[F] with OptionTFunctor[F] { implicit val F = F0 }

  implicit def catsDataOrderForOptionT[F[_], A](implicit F0: Order[F[Option[A]]]): Order[OptionT[F, A]] =
    new OptionTOrder[F, A] { implicit val F = F0 }

  implicit def catsDataMonoidForOptionT[F[_], A](implicit F0: Monoid[F[Option[A]]]): Monoid[OptionT[F, A]] =
    new OptionTMonoid[F, A] { implicit val F = F0 }

  implicit def catsDataShowForOptionT[F[_], A](implicit F: Show[F[Option[A]]]): Show[OptionT[F, A]] =
    Contravariant[Show].contramap(F)(_.value)

  implicit def catsDataDeferForOptionT[F[_]](implicit F: Defer[F]): Defer[OptionT[F, *]] =
    new Defer[OptionT[F, *]] {
      def defer[A](fa: => OptionT[F, A]): OptionT[F, A] =
        OptionT(F.defer(fa.value))
    }

  implicit def catsDataTraverseFilterForOptionT[F[_]](implicit F0: Traverse[F]): TraverseFilter[OptionT[F, *]] =
    new OptionTFunctorFilter[F] with TraverseFilter[OptionT[F, *]] {
      implicit def F: Functor[F] = F0

      val traverse: Traverse[OptionT[F, *]] = OptionT.catsDataTraverseForOptionT[F]

      def traverseFilter[G[_], A, B](
        fa: OptionT[F, A]
      )(f: A => G[Option[B]])(implicit G: Applicative[G]): G[OptionT[F, B]] =
        G.map(Traverse[F].traverse[G, Option[A], Option[B]](fa.value) { oa =>
          TraverseFilter[Option].traverseFilter(oa)(f)
        })(OptionT[F, B])

      override def filterA[G[_], A](
        fa: OptionT[F, A]
      )(f: A => G[Boolean])(implicit G: Applicative[G]): G[OptionT[F, A]] =
        G.map(Traverse[F].traverse(fa.value)(TraverseFilter[Option].filterA[G, A](_)(f)))(OptionT[F, A])

    }

  @deprecated("renamed to catsDataTraverseFilterForOptionT", "2.0.0")
  def catsDateTraverseFilterForOptionT[F[_]](implicit F0: Traverse[F]): TraverseFilter[OptionT[F, *]] =
    catsDataTraverseFilterForOptionT

  implicit def catsDataParallelForOptionT[M[_]](implicit
    P: Parallel[M]
  ): Parallel.Aux[OptionT[M, *], Nested[P.F, Option, *]] =
    new Parallel[OptionT[M, *]] {
      type F[x] = Nested[P.F, Option, x]

      implicit val monadM: Monad[M] = P.monad

      def applicative: Applicative[Nested[P.F, Option, *]] =
        cats.data.Nested.catsDataApplicativeForNested(P.applicative, cats.instances.option.catsStdInstancesForOption)

      def monad: Monad[OptionT[M, *]] = cats.data.OptionT.catsDataMonadErrorMonadForOptionT[M]

      def sequential: Nested[P.F, Option, *] ~> OptionT[M, *] =
        new (Nested[P.F, Option, *] ~> OptionT[M, *]) {
          def apply[A](nested: Nested[P.F, Option, A]): OptionT[M, A] = OptionT(P.sequential(nested.value))
        }

      def parallel: OptionT[M, *] ~> Nested[P.F, Option, *] =
        new (OptionT[M, *] ~> Nested[P.F, Option, *]) {
          def apply[A](optT: OptionT[M, A]): Nested[P.F, Option, A] = Nested(P.parallel(optT.value))
        }
    }
}

sealed abstract private[data] class OptionTInstances0 extends OptionTInstances1 {
  // the Dummy type is to make this one more specific than catsDataMonadErrorMonadForOptionT on 2.13.x
  // see https://github.com/typelevel/cats/pull/2335#issuecomment-408249775
  implicit def catsDataMonadErrorForOptionT[F[_], E](implicit
    F0: MonadError[F, E]
  ): MonadError[OptionT[F, *], E] { type Dummy } =
    new OptionTMonadError[F, E] {
      type Dummy
      implicit val F = F0
    }

  implicit def catsDataMonoidKForOptionT[F[_]](implicit F0: Monad[F]): MonoidK[OptionT[F, *]] =
    new OptionTMonoidK[F] { implicit val F = F0 }

  implicit def catsDataSemigroupForOptionT[F[_], A](implicit F0: Semigroup[F[Option[A]]]): Semigroup[OptionT[F, A]] =
    new OptionTSemigroup[F, A] { implicit val F = F0 }

  implicit def catsDataPartialOrderForOptionT[F[_], A](implicit
    F0: PartialOrder[F[Option[A]]]
  ): PartialOrder[OptionT[F, A]] =
    new OptionTPartialOrder[F, A] { implicit val F = F0 }

  implicit def catsDateFunctorFilterForOptionT[F[_]](implicit F0: Functor[F]): FunctorFilter[OptionT[F, *]] =
    new OptionTFunctorFilter[F] { implicit val F = F0 }

  implicit def catsDataContravariantForOptionT[F[_]](implicit F0: Contravariant[F]): Contravariant[OptionT[F, *]] =
    new OptionTContravariant[F] { implicit val F = F0 }
}

sealed abstract private[data] class OptionTInstances1 extends OptionTInstances2 {
  implicit def catsDataSemigroupKForOptionT[F[_]](implicit F0: Monad[F]): SemigroupK[OptionT[F, *]] =
    new OptionTSemigroupK[F] { implicit val F = F0 }

  implicit def catsDataEqForOptionT[F[_], A](implicit F0: Eq[F[Option[A]]]): Eq[OptionT[F, A]] =
    new OptionTEq[F, A] { implicit val F = F0 }

  implicit def catsDataMonadErrorMonadForOptionT[F[_]](implicit F0: Monad[F]): MonadError[OptionT[F, *], Unit] =
    new OptionTMonadErrorMonad[F] { implicit val F = F0 }

  implicit def catsDataContravariantMonoidalForOptionT[F[_]](implicit
    F0: ContravariantMonoidal[F]
  ): ContravariantMonoidal[OptionT[F, ?]] =
    new OptionTContravariantMonoidal[F] { implicit val F = F0 }
}

sealed abstract private[data] class OptionTInstances2 extends OptionTInstances3 {
  implicit def catsDataFoldableForOptionT[F[_]](implicit F0: Foldable[F]): Foldable[OptionT[F, *]] =
    new OptionTFoldable[F] { implicit val F = F0 }

  implicit def catsDataInvariantForOptionT[F[_]](implicit F0: Invariant[F]): Invariant[OptionT[F, *]] =
    new OptionTInvariant[F] { implicit val F = F0 }
}

sealed abstract private[data] class OptionTInstances3 {
  implicit def catsDataFunctorForOptionT[F[_]](implicit F0: Functor[F]): Functor[OptionT[F, *]] =
    new OptionTFunctor[F] { implicit val F = F0 }
}

private[data] trait OptionTFunctor[F[_]] extends Functor[OptionT[F, *]] {
  implicit def F: Functor[F]

  override def map[A, B](fa: OptionT[F, A])(f: A => B): OptionT[F, B] = fa.map(f)
}

sealed private[data] trait OptionTInvariant[F[_]] extends Invariant[OptionT[F, *]] {
  implicit def F: Invariant[F]

  override def imap[A, B](fa: OptionT[F, A])(f: A => B)(g: B => A): OptionT[F, B] =
    fa.imap(f)(g)
}

sealed private[data] trait OptionTContravariant[F[_]] extends Contravariant[OptionT[F, *]] {
  implicit def F: Contravariant[F]

  override def contramap[A, B](fa: OptionT[F, A])(f: B => A): OptionT[F, B] =
    fa.contramap(f)
}

private[data] trait OptionTMonad[F[_]] extends Monad[OptionT[F, *]] {
  implicit def F: Monad[F]

  def pure[A](a: A): OptionT[F, A] = OptionT.pure(a)

  def flatMap[A, B](fa: OptionT[F, A])(f: A => OptionT[F, B]): OptionT[F, B] = fa.flatMap(f)

  override def map[A, B](fa: OptionT[F, A])(f: A => B): OptionT[F, B] = fa.map(f)

  def tailRecM[A, B](a: A)(f: A => OptionT[F, Either[A, B]]): OptionT[F, B] =
    OptionT(
      F.tailRecM(a)(a0 =>
        F.map(f(a0).value)(
          _.fold[Either[A, Option[B]]](Right(None))(_.map(b => Some(b): Option[B]))
        )
      )
    )
}

private[data] trait OptionTMonadErrorMonad[F[_]] extends MonadError[OptionT[F, *], Unit] with OptionTMonad[F] {
  implicit def F: Monad[F]

  override def raiseError[A](e: Unit): OptionT[F, A] = OptionT.none

  override def handleErrorWith[A](fa: OptionT[F, A])(f: Unit => OptionT[F, A]): OptionT[F, A] =
    OptionT(F.flatMap(fa.value) {
      case s @ Some(_) => F.pure(s)
      case None        => f(()).value
    })
}

private trait OptionTMonadError[F[_], E] extends MonadError[OptionT[F, *], E] with OptionTMonad[F] {
  override def F: MonadError[F, E]

  override def raiseError[A](e: E): OptionT[F, A] =
    OptionT(F.map(F.raiseError[A](e))(Some(_)))

  override def handleErrorWith[A](fa: OptionT[F, A])(f: E => OptionT[F, A]): OptionT[F, A] =
    OptionT(F.handleErrorWith(fa.value)(f(_).value))
}

private trait OptionTContravariantMonoidal[F[_]] extends ContravariantMonoidal[OptionT[F, *]] {
  def F: ContravariantMonoidal[F]

  override def unit: OptionT[F, Unit] = OptionT(F.trivial)

  override def contramap[A, B](fa: OptionT[F, A])(f: B => A): OptionT[F, B] =
    OptionT(F.contramap(fa.value)(_.map(f)))

  override def product[A, B](fa: OptionT[F, A], fb: OptionT[F, B]): OptionT[F, (A, B)] =
    OptionT(
      F.contramap(F.product(fa.value, fb.value))({
        case Some((x, y)) => (Some(x), Some(y))
        case None         => (None, None)
      })
    )
}

private[data] trait OptionTFoldable[F[_]] extends Foldable[OptionT[F, *]] {
  implicit def F: Foldable[F]

  def foldLeft[A, B](fa: OptionT[F, A], b: B)(f: (B, A) => B): B =
    fa.foldLeft(b)(f)

  def foldRight[A, B](fa: OptionT[F, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    fa.foldRight(lb)(f)
}

sealed private[data] trait OptionTTraverse[F[_]] extends Traverse[OptionT[F, *]] with OptionTFoldable[F] {
  implicit def F: Traverse[F]

  def traverse[G[_]: Applicative, A, B](fa: OptionT[F, A])(f: A => G[B]): G[OptionT[F, B]] =
    fa.traverse(f)
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

private[data] trait OptionTSemigroupK[F[_]] extends SemigroupK[OptionT[F, *]] {
  implicit def F: Monad[F]

  def combineK[A](x: OptionT[F, A], y: OptionT[F, A]): OptionT[F, A] = x.orElse(y)

  override def combineKEval[A](x: OptionT[F, A], y: Eval[OptionT[F, A]]): Eval[OptionT[F, A]] =
    Eval.now(OptionT(F.flatMap(x.value) {
      case oa @ Some(_) => F.pure(oa)
      case None         => y.value.value
    }))
}

private[data] trait OptionTMonoidK[F[_]] extends MonoidK[OptionT[F, *]] with OptionTSemigroupK[F] {
  def empty[A]: OptionT[F, A] = OptionT.none[F, A]
}

sealed private[data] trait OptionTEq[F[_], A] extends Eq[OptionT[F, A]] {
  implicit def F: Eq[F[Option[A]]]

  override def eqv(x: OptionT[F, A], y: OptionT[F, A]): Boolean = x === y
}

sealed private[data] trait OptionTPartialOrder[F[_], A] extends PartialOrder[OptionT[F, A]] with OptionTEq[F, A] {
  implicit override def F: PartialOrder[F[Option[A]]]

  override def partialCompare(x: OptionT[F, A], y: OptionT[F, A]): Double = x.partialCompare(y)
}

sealed private[data] trait OptionTFunctorFilter[F[_]] extends FunctorFilter[OptionT[F, *]] {
  implicit def F: Functor[F]

  def functor: Functor[OptionT[F, *]] = OptionT.catsDataFunctorForOptionT[F]

  def mapFilter[A, B](fa: OptionT[F, A])(f: (A) => Option[B]): OptionT[F, B] = fa.subflatMap(f)

  override def collect[A, B](fa: OptionT[F, A])(f: PartialFunction[A, B]): OptionT[F, B] = fa.subflatMap(f.lift)

  override def flattenOption[A](fa: OptionT[F, Option[A]]): OptionT[F, A] = fa.subflatMap(identity)

  override def filter[A](fa: OptionT[F, A])(f: (A) => Boolean): OptionT[F, A] = fa.filter(f)

  override def filterNot[A](fa: OptionT[F, A])(f: A => Boolean): OptionT[F, A] = fa.filterNot(f)
}

sealed private[data] trait OptionTOrder[F[_], A] extends Order[OptionT[F, A]] with OptionTPartialOrder[F, A] {
  implicit override def F: Order[F[Option[A]]]

  override def compare(x: OptionT[F, A], y: OptionT[F, A]): Int = x.compare(y)
}
