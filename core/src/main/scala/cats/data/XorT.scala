package cats
package data

import cats.functor.Bifunctor

/**
 * Transformer for `Xor`, allowing the effect of an arbitrary type constructor `F` to be combined with the
 * fail-fast effect of `Xor`.
 *
 * `XorT[F, A, B]` wraps a value of type `F[A Xor B]`. An `F[C]` can be lifted in to `XorT[F, A, C]` via `XorT.right`,
 * and lifted in to a `XorT[F, C, B]` via `XorT.left`.
 */
final case class XorT[F[_], A, B](value: F[A Xor B]) {

  def fold[C](fa: A => C, fb: B => C)(implicit F: Functor[F]): F[C] = F.map(value)(_.fold(fa, fb))

  def isLeft(implicit F: Functor[F]): F[Boolean] = F.map(value)(_.isLeft)

  def isRight(implicit F: Functor[F]): F[Boolean] = F.map(value)(_.isRight)

  def swap(implicit F: Functor[F]): XorT[F, B, A] = XorT(F.map(value)(_.swap))

  def getOrElse[BB >: B](default: => BB)(implicit F: Functor[F]): F[BB] = F.map(value)(_.getOrElse(default))

  def getOrElseF[BB >: B](default: => F[BB])(implicit F: Monad[F]): F[BB] = {
    F.flatMap(value) {
      case Xor.Left(_) => default
      case Xor.Right(b) => F.pure(b)
    }
  }

  def orElse[AA, BB >: B](default: => XorT[F, AA, BB])(implicit F: Monad[F]): XorT[F, AA, BB] = {
    XorT(F.flatMap(value) {
      case Xor.Left(_) => default.value
      case r @ Xor.Right(_) => F.pure(r)
    })
  }

  def recover(pf: PartialFunction[A, B])(implicit F: Functor[F]): XorT[F, A, B] =
    XorT(F.map(value)(_.recover(pf)))

  def recoverWith(pf: PartialFunction[A, XorT[F, A, B]])(implicit F: Monad[F]): XorT[F, A, B] =
    XorT(F.flatMap(value) {
      case Xor.Left(a) if pf.isDefinedAt(a) => pf(a).value
      case other => F.pure(other)
    })

  def valueOr[BB >: B](f: A => BB)(implicit F: Functor[F]): F[BB] = fold(f, identity)

  def forall(f: B => Boolean)(implicit F: Functor[F]): F[Boolean] = F.map(value)(_.forall(f))

  def exists(f: B => Boolean)(implicit F: Functor[F]): F[Boolean] = F.map(value)(_.exists(f))

  def ensure[AA >: A](onFailure: => AA)(f: B => Boolean)(implicit F: Functor[F]): XorT[F, AA, B] = XorT(F.map(value)(_.ensure(onFailure)(f)))

  def toEither(implicit F: Functor[F]): F[Either[A, B]] = F.map(value)(_.toEither)

  def toEitherT(implicit F: Functor[F]): EitherT[F, A, B] = EitherT(toEither)

  def toOption(implicit F: Functor[F]): OptionT[F, B] = OptionT(F.map(value)(_.toOption))

  def to[G[_]](implicit F: Functor[F], G: Alternative[G]): F[G[B]] =
    F.map(value)(_.to[G, B])

  def collectRight(implicit F: MonadCombine[F]): F[B] =
    F.flatMap(value)(_.to[F, B])

  def bimap[C, D](fa: A => C, fb: B => D)(implicit F: Functor[F]): XorT[F, C, D] = XorT(F.map(value)(_.bimap(fa, fb)))

  def bitraverse[G[_], C, D](f: A => G[C], g: B => G[D])(implicit traverseF: Traverse[F], applicativeG: Applicative[G]): G[XorT[F, C, D]] =
    applicativeG.map(traverseF.traverse(value)(axb => Bitraverse[Xor].bitraverse(axb)(f, g)))(XorT.apply)

  def applyAlt[D](ff: XorT[F, A, B => D])(implicit F: Apply[F]): XorT[F, A, D] =
    XorT[F, A, D](F.map2(this.value, ff.value)((xb, xbd) => Apply[A Xor ?].ap(xbd)(xb)))

  def flatMap[AA >: A, D](f: B => XorT[F, AA, D])(implicit F: Monad[F]): XorT[F, AA, D] =
    XorT(F.flatMap(value) {
      case l @ Xor.Left(_) => F.pure(l)
      case Xor.Right(b) => f(b).value
    })

  def flatMapF[AA >: A, D](f: B => F[AA Xor D])(implicit F: Monad[F]): XorT[F, AA, D] =
    flatMap(f andThen XorT.apply)

  def transform[C, D](f: Xor[A, B] => Xor[C, D])(implicit F: Functor[F]): XorT[F, C, D] =
    XorT(F.map(value)(f))

  def subflatMap[AA >: A, D](f: B => AA Xor D)(implicit F: Functor[F]): XorT[F, AA, D] =
    transform(_.flatMap(f))

  def map[D](f: B => D)(implicit F: Functor[F]): XorT[F, A, D] = bimap(identity, f)

  def semiflatMap[D](f: B => F[D])(implicit F: Monad[F]): XorT[F, A, D] =
    flatMap(b => XorT.right[F, A, D](f(b)))

  def leftMap[C](f: A => C)(implicit F: Functor[F]): XorT[F, C, B] = bimap(f, identity)

  def compare(that: XorT[F, A, B])(implicit o: Order[F[A Xor B]]): Int =
    o.compare(value, that.value)

  def partialCompare(that: XorT[F, A, B])(implicit p: PartialOrder[F[A Xor B]]): Double =
    p.partialCompare(value, that.value)

  def ===(that: XorT[F, A, B])(implicit eq: Eq[F[A Xor B]]): Boolean =
    eq.eqv(value, that.value)

  def traverse[G[_], D](f: B => G[D])(implicit traverseF: Traverse[F], applicativeG: Applicative[G]): G[XorT[F, A, D]] =
    applicativeG.map(traverseF.traverse(value)(axb => Traverse[A Xor ?].traverse(axb)(f)))(XorT.apply)

  def foldLeft[C](c: C)(f: (C, B) => C)(implicit F: Foldable[F]): C =
    F.foldLeft(value, c)((c, axb) => axb.foldLeft(c)(f))

  def foldRight[C](lc: Eval[C])(f: (B, Eval[C]) => Eval[C])(implicit F: Foldable[F]): Eval[C] =
    F.foldRight(value, lc)((axb, lc) => axb.foldRight(lc)(f))

  def merge[AA >: A](implicit ev: B <:< AA, F: Functor[F]): F[AA] = F.map(value)(_.fold(identity, ev.apply))

  /**
   * Similar to [[Xor.combine]] but mapped over an `F` context.
   *
   * Examples:
   * {{{
   * scala> import cats.data.XorT
   * scala> import cats.implicits._
   * scala> val l1: XorT[Option, String, Int] = XorT.left(Some("error 1"))
   * scala> val l2: XorT[Option, String, Int] = XorT.left(Some("error 2"))
   * scala> val r3: XorT[Option, String, Int] = XorT.right(Some(3))
   * scala> val r4: XorT[Option, String, Int] = XorT.right(Some(4))
   * scala> val noneXorT: XorT[Option, String, Int] = XorT.left(None)
   *
   * scala> l1 combine l2
   * res0: XorT[Option, String, Int] = XorT(Some(Left(error 1)))
   *
   * scala> l1 combine r3
   * res1: XorT[Option, String, Int] = XorT(Some(Left(error 1)))
   *
   * scala> r3 combine l1
   * res2: XorT[Option, String, Int] = XorT(Some(Left(error 1)))
   *
   * scala> r3 combine r4
   * res3: XorT[Option, String, Int] = XorT(Some(Right(7)))
   *
   * scala> l1 combine noneXorT
   * res4: XorT[Option, String, Int] = XorT(None)
   *
   * scala> noneXorT combine l1
   * res5: XorT[Option, String, Int] = XorT(None)
   *
   * scala> r3 combine noneXorT
   * res6: XorT[Option, String, Int] = XorT(None)
   *
   * scala> noneXorT combine r4
   * res7: XorT[Option, String, Int] = XorT(None)
   * }}}
   */
  def combine(that: XorT[F, A, B])(implicit F: Apply[F], B: Semigroup[B]): XorT[F, A, B] =
    XorT(F.map2(this.value, that.value)(_ combine _))

  def toValidated(implicit F: Functor[F]): F[Validated[A, B]] =
    F.map(value)(_.toValidated)

  def toValidatedNel(implicit F: Functor[F]): F[ValidatedNel[A, B]] =
    F.map(value)(_.toValidatedNel)

  /** Run this value as a `[[Validated]]` against the function and convert it back to an `[[XorT]]`.
   *
   * The [[Applicative]] instance for `XorT` "fails fast" - it is often useful to "momentarily" have
   * it accumulate errors instead, which is what the `[[Validated]]` data type gives us.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> type Error = String
   * scala> val v1: Validated[NonEmptyList[Error], Int] = Validated.Invalid(NonEmptyList.of("error 1"))
   * scala> val v2: Validated[NonEmptyList[Error], Int] = Validated.Invalid(NonEmptyList.of("error 2"))
   * scala> val xort: XorT[Option, Error, Int] = XorT(Some(Xor.left("error 3")))
   * scala> xort.withValidated { v3 => (v1 |@| v2 |@| v3.leftMap(NonEmptyList.of(_))).map{ case (i, j, k) => i + j + k } }
   * res0: XorT[Option, NonEmptyList[Error], Int] = XorT(Some(Left(NonEmptyList(error 1, error 2, error 3))))
   * }}}
   */
  def withValidated[AA, BB](f: Validated[A, B] => Validated[AA, BB])(implicit F: Functor[F]): XorT[F, AA, BB] =
    XorT(F.map(value)(xor => f(xor.toValidated).toXor))

  def show(implicit show: Show[F[A Xor B]]): String = show.show(value)

  /**
   * Transform this `XorT[F, A, B]` into a `[[Nested]][F, A Xor ?, B]`.
   *
   * An example where `toNested` can be used, is to get the `Apply.ap` function with the
   * behavior from the composed `Apply` instances from `F` and `Xor[A, ?]`, which is
   * inconsistent with the behavior of the `ap` from `Monad` of `XorT`.
   *
   * {{{
   * scala> import cats.data.{Nested, Xor, XorT}
   * scala> import cats.implicits._
   * scala> val ff: XorT[List, String, Int => String] =
   *      |   XorT(List(Xor.right(_.toString), Xor.left("error")))
   * scala> val fa: XorT[List, String, Int] =
   *      |   XorT(List(Xor.right(1), Xor.right(2)))
   * scala> type ErrorOr[A] = String Xor A
   * scala> type ListErrorOr[A] = Nested[List, ErrorOr, A]
   * scala> ff.ap(fa)
   * res0: XorT[List,String,String] = XorT(List(Right(1), Right(2), Left(error)))
   * scala> XorT((ff.toNested: ListErrorOr[Int => String]).ap(fa.toNested: ListErrorOr[Int]).value)
   * res1: XorT[List,String,String] = XorT(List(Right(1), Right(2), Left(error), Left(error)))
   * }}}
   *
   * Note that we need the `ErrorOr` type alias above because otherwise we can't use the
   * syntax function `ap` on `Nested[List, A Xor ?, B]`. This won't be needed after cats has
   * decided [[https://github.com/typelevel/cats/issues/1073 how to handle the SI-2712 fix]].
   */
  def toNested: Nested[F, A Xor ?, B] = Nested[F, A Xor ?, B](value)
}

object XorT extends XorTInstances with XorTFunctions

trait XorTFunctions {
  final def left[F[_], A, B](fa: F[A])(implicit F: Functor[F]): XorT[F, A, B] = XorT(F.map(fa)(Xor.left))

  final def right[F[_], A, B](fb: F[B])(implicit F: Functor[F]): XorT[F, A, B] = XorT(F.map(fb)(Xor.right))

  final def pure[F[_], A, B](b: B)(implicit F: Applicative[F]): XorT[F, A, B] = right(F.pure(b))

  /**
   * Alias for [[XorT.right]]
   * {{{
   * scala> import cats.data.XorT
   * scala> import cats.implicits._
   * scala> val o: Option[Int] = Some(3)
   * scala> val n: Option[Int] = None
   * scala> XorT.liftT(o)
   * res0: cats.data.XorT[Option,Nothing,Int] = XorT(Some(Right(3)))
   * scala> XorT.liftT(n)
   * res1: cats.data.XorT[Option,Nothing,Int] = XorT(None)
   * }}}
   */
  final def liftT[F[_], A, B](fb: F[B])(implicit F: Functor[F]): XorT[F, A, B] = right(fb)

  /** Transforms an `Xor` into an `XorT`, lifted into the specified `Applicative`.
   *
   * Note: The return type is a FromXorPartiallyApplied[F], which has an apply method
   * on it, allowing you to call fromXor like this:
   * {{{
   * scala> import cats.implicits._
   * scala> val t: Xor[String, Int] = Xor.right(3)
   * scala> XorT.fromXor[Option](t)
   * res0: XorT[Option, String, Int] = XorT(Some(Right(3)))
   * }}}
   *
   * The reason for the indirection is to emulate currying type parameters.
   */
  final def fromXor[F[_]]: FromXorPartiallyApplied[F] = new FromXorPartiallyApplied

  final class FromXorPartiallyApplied[F[_]] private[XorTFunctions] {
    def apply[E, A](xor: Xor[E, A])(implicit F: Applicative[F]): XorT[F, E, A] =
      XorT(F.pure(xor))
  }

  final def fromEither[F[_]]: FromEitherPartiallyApplied[F] = new FromEitherPartiallyApplied

  final class FromEitherPartiallyApplied[F[_]] private[XorTFunctions] {
    def apply[E, A](eit: Either[E, A])(implicit F: Applicative[F]): XorT[F, E, A] =
      XorT(F.pure(Xor.fromEither(eit)))
  }
}

private[data] abstract class XorTInstances extends XorTInstances1 {

  /* TODO violates right absorbtion, right distributivity, and left distributivity -- re-enable when MonadCombine laws are split in to weak/strong
  implicit def catsDataMonadCombineForXorT[F[_], L](implicit F: Monad[F], L: Monoid[L]): MonadCombine[XorT[F, L, ?]] = {
    implicit val F0 = F
    implicit val L0 = L
    new XorTMonadCombine[F, L] { implicit val F = F0; implicit val L = L0 }
  }
  */

  implicit def catsDataOrderForXorT[F[_], L, R](implicit F: Order[F[L Xor R]]): Order[XorT[F, L, R]] =
    new XorTOrder[F, L, R] {
      val F0: Order[F[L Xor R]] = F
    }

  implicit def catsDataShowForXorT[F[_], L, R](implicit sh: Show[F[L Xor R]]): Show[XorT[F, L, R]] =
    functor.Contravariant[Show].contramap(sh)(_.value)

  implicit def catsDataBifunctorForXorT[F[_]](implicit F: Functor[F]): Bifunctor[XorT[F, ?, ?]] =
    new Bifunctor[XorT[F, ?, ?]] {
      override def bimap[A, B, C, D](fab: XorT[F, A, B])(f: A => C, g: B => D): XorT[F, C, D] = fab.bimap(f, g)
    }

  implicit def catsDataTraverseForXorT[F[_], L](implicit F: Traverse[F]): Traverse[XorT[F, L, ?]] =
    new XorTTraverse[F, L] {
      val F0: Traverse[F] = F
    }

  implicit def catsDataTransLiftForXorT[E]: TransLift.Aux[XorT[?[_], E, ?], Functor] =
    new TransLift[XorT[?[_], E, ?]] {
      type TC[M[_]] = Functor[M]

      def liftT[M[_]: Functor, A](ma: M[A]): XorT[M, E, A] =
        XorT.liftT(ma)
    }

  implicit def catsMonoidForXorT[F[_], L, A](implicit F: Monoid[F[L Xor A]]): Monoid[XorT[F, L, A]] =
    new XorTMonoid[F, L, A] { implicit val F0 = F }

}

private[data] abstract class XorTInstances1 extends XorTInstances2 {
  /* TODO violates monadFilter right empty law -- re-enable when MonadFilter laws are split in to weak/strong
  implicit def catsDataMonadFilterForXorT[F[_], L](implicit F: Monad[F], L: Monoid[L]): MonadFilter[XorT[F, L, ?]] = {
    implicit val F0 = F
    implicit val L0 = L
    new XorTMonadFilter[F, L] { implicit val F = F0; implicit val L = L0 }
  }
   */

  implicit def catsSemigroupForXorT[F[_], L, A](implicit F: Semigroup[F[L Xor A]]): Semigroup[XorT[F, L, A]] =
    new XorTSemigroup[F, L, A] { implicit val F0 = F }

  implicit def catsDataFoldableForXorT[F[_], L](implicit F: Foldable[F]): Foldable[XorT[F, L, ?]] =
    new XorTFoldable[F, L] {
      val F0: Foldable[F] = F
    }

  implicit def catsDataPartialOrderForXorT[F[_], L, R](implicit F: PartialOrder[F[L Xor R]]): PartialOrder[XorT[F, L, R]] =
    new XorTPartialOrder[F, L, R] {
      val F0: PartialOrder[F[L Xor R]] = F
    }

  implicit def catsDataBitraverseForXorT[F[_]](implicit F: Traverse[F]): Bitraverse[XorT[F, ?, ?]] =
    new XorTBitraverse[F] {
      val F0: Traverse[F] = F
    }
}

private[data] abstract class XorTInstances2 extends XorTInstances3 {
  implicit def catsDataMonadErrorForXorT[F[_], L](implicit F0: Monad[F]): MonadError[XorT[F, L, ?], L] =
    new XorTMonadError[F, L] { implicit val F = F0 }

  implicit def catsDataRecursiveTailRecMForXorT[F[_]: RecursiveTailRecM, L]: RecursiveTailRecM[XorT[F, L, ?]] =
    RecursiveTailRecM.create[XorT[F, L, ?]]

  implicit def catsDataSemigroupKForXorT[F[_], L](implicit F0: Monad[F]): SemigroupK[XorT[F, L, ?]] =
    new XorTSemigroupK[F, L] { implicit val F = F0 }

  implicit def catsDataEqForXorT[F[_], L, R](implicit F: Eq[F[L Xor R]]): Eq[XorT[F, L, R]] =
    new XorTEq[F, L, R] {
      val F0: Eq[F[L Xor R]] = F
    }
}

private[data] abstract class XorTInstances3 {
  implicit def catsDataFunctorForXorT[F[_], L](implicit F0: Functor[F]): Functor[XorT[F, L, ?]] =
    new XorTFunctor[F, L] { implicit val F = F0 }
}

private[data] trait XorTSemigroup[F[_], L, A] extends Semigroup[XorT[F, L, A]] {
  implicit val F0: Semigroup[F[L Xor A]]
  def combine(x: XorT[F, L , A], y: XorT[F, L , A]): XorT[F, L , A] =
    XorT(F0.combine(x.value, y.value))
}

private[data] trait XorTMonoid[F[_], L, A] extends Monoid[XorT[F, L, A]] with XorTSemigroup[F, L, A] {
  implicit val F0: Monoid[F[L Xor A]]
  def empty: XorT[F, L, A] = XorT(F0.empty)
}

private[data] trait XorTSemigroupK[F[_], L] extends SemigroupK[XorT[F, L, ?]] {
  implicit val F: Monad[F]
  def combineK[A](x: XorT[F, L, A], y: XorT[F, L, A]): XorT[F, L, A] =
    XorT(F.flatMap(x.value) {
      case l @ Xor.Left(_) => y.value
      case r @ Xor.Right(_) => F.pure(r)
    })
}

private[data] trait XorTFunctor[F[_], L] extends Functor[XorT[F, L, ?]] {
  implicit val F: Functor[F]
  override def map[A, B](fa: XorT[F, L, A])(f: A => B): XorT[F, L, B] = fa map f
}

private[data] trait XorTMonad[F[_], L] extends Monad[XorT[F, L, ?]] with XorTFunctor[F, L] {
  implicit val F: Monad[F]
  def pure[A](a: A): XorT[F, L, A] = XorT(F.pure(Xor.right(a)))
  def flatMap[A, B](fa: XorT[F, L, A])(f: A => XorT[F, L, B]): XorT[F, L, B] = fa flatMap f
  def tailRecM[A, B](a: A)(f: A => XorT[F, L, Either[A, B]]): XorT[F, L, B] =
    XorT(F.tailRecM(a)(a0 => F.map(f(a0).value) {
      case Xor.Left(l)         => Right(Xor.Left(l))
      case Xor.Right(Left(a1)) => Left(a1)
      case Xor.Right(Right(b)) => Right(Xor.Right(b))
    }))
}

private[data] trait XorTMonadError[F[_], L] extends MonadError[XorT[F, L, ?], L] with XorTMonad[F, L] {
  def handleErrorWith[A](fea: XorT[F, L, A])(f: L => XorT[F, L, A]): XorT[F, L, A] =
    XorT(F.flatMap(fea.value) {
      case Xor.Left(e) => f(e).value
      case r @ Xor.Right(_) => F.pure(r)
    })
  override def handleError[A](fea: XorT[F, L, A])(f: L => A): XorT[F, L, A] =
    XorT(F.flatMap(fea.value) {
      case Xor.Left(e) => F.pure(Xor.Right(f(e)))
      case r @ Xor.Right(_) => F.pure(r)
    })
  def raiseError[A](e: L): XorT[F, L, A] = XorT.left(F.pure(e))
  override def attempt[A](fla: XorT[F, L, A]): XorT[F, L, Xor[L, A]] = XorT.right(fla.value)
  override def recover[A](fla: XorT[F, L, A])(pf: PartialFunction[L, A]): XorT[F, L, A] =
    fla.recover(pf)
  override def recoverWith[A](fla: XorT[F, L, A])(pf: PartialFunction[L, XorT[F, L, A]]): XorT[F, L, A] =
    fla.recoverWith(pf)
}

private[data] trait XorTMonadFilter[F[_], L] extends MonadFilter[XorT[F, L, ?]] with XorTMonadError[F, L] {
  implicit val F: Monad[F]
  implicit val L: Monoid[L]
  def empty[A]: XorT[F, L, A] = XorT(F.pure(Xor.left(L.empty)))
}

/* TODO violates right absorbtion, right distributivity, and left distributivity -- re-enable when MonadCombine laws are split in to weak/strong
private[data] trait XorTMonadCombine[F[_], L] extends MonadCombine[XorT[F, L, ?]] with XorTMonadFilter[F, L] with XorTSemigroupK[F, L] {
  implicit val F: Monad[F]
  implicit val L: Monoid[L]
}
*/

private[data] sealed trait XorTFoldable[F[_], L] extends Foldable[XorT[F, L, ?]] {
  implicit def F0: Foldable[F]

  def foldLeft[A, B](fa: XorT[F, L, A], b: B)(f: (B, A) => B): B =
    fa.foldLeft(b)(f)

  def foldRight[A, B](fa: XorT[F, L, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    fa.foldRight(lb)(f)
}

private[data] sealed trait XorTTraverse[F[_], L] extends Traverse[XorT[F, L, ?]] with XorTFoldable[F, L] {
  override implicit def F0: Traverse[F]

  override def traverse[G[_]: Applicative, A, B](fa: XorT[F, L, A])(f: A => G[B]): G[XorT[F, L, B]] =
    fa traverse f
}

private[data] sealed trait XorTBifoldable[F[_]] extends Bifoldable[XorT[F, ?, ?]] {
  implicit def F0: Foldable[F]

  def bifoldLeft[A, B, C](fab: XorT[F, A, B], c: C)(f: (C, A) => C, g: (C, B) => C): C =
    F0.foldLeft(fab.value, c)( (acc, axb) => Bifoldable[Xor].bifoldLeft(axb, acc)(f, g))

  def bifoldRight[A, B, C](fab: XorT[F, A, B], c: Eval[C])(f: (A, Eval[C]) => Eval[C], g: (B, Eval[C]) => Eval[C]): Eval[C] =
    F0.foldRight(fab.value, c)( (axb, acc) => Bifoldable[Xor].bifoldRight(axb, acc)(f, g))
}

private[data] sealed trait XorTBitraverse[F[_]] extends Bitraverse[XorT[F, ?, ?]] with XorTBifoldable[F] {
  override implicit def F0: Traverse[F]

  override def bitraverse[G[_], A, B, C, D](fab: XorT[F, A, B])(f: A => G[C], g: B => G[D])(implicit G: Applicative[G]): G[XorT[F, C, D]] =
    fab.bitraverse(f, g)
}

private[data] sealed trait XorTEq[F[_], L, A] extends Eq[XorT[F, L, A]] {
  implicit def F0: Eq[F[L Xor A]]

  override def eqv(x: XorT[F, L, A], y: XorT[F, L, A]): Boolean = x === y
}

private[data] sealed trait XorTPartialOrder[F[_], L, A] extends PartialOrder[XorT[F, L, A]] with XorTEq[F, L, A]{
  override implicit def F0: PartialOrder[F[L Xor A]]

  override def partialCompare(x: XorT[F, L, A], y: XorT[F, L, A]): Double =
    x partialCompare y
}

private[data] sealed trait XorTOrder[F[_], L, A] extends Order[XorT[F, L, A]] with XorTPartialOrder[F, L, A]{
  override implicit def F0: Order[F[L Xor A]]

  override def compare(x: XorT[F, L, A], y: XorT[F, L, A]): Int = x compare y
}
