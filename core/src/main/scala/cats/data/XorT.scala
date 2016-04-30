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

  def toOption(implicit F: Functor[F]): OptionT[F, B] = OptionT(F.map(value)(_.toOption))

  def to[G[_]](implicit functorF: Functor[F], monoidKG: MonoidK[G], applicativeG: Applicative[G]): F[G[B]] =
    functorF.map(value)(_.to[G, B])

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

  def combine(that: XorT[F, A, B])(implicit F: Apply[F], A: Semigroup[A], B: Semigroup[B]): XorT[F, A, B] =
    XorT(F.map2(this.value, that.value)(_ combine _))

  def toValidated(implicit F: Functor[F]): F[Validated[A, B]] =
    F.map(value)(_.toValidated)

  /** Run this value as a `[[Validated]]` against the function and convert it back to an `[[XorT]]`.
   *
   * The [[Applicative]] instance for `XorT` "fails fast" - it is often useful to "momentarily" have
   * it accumulate errors instead, which is what the `[[Validated]]` data type gives us.
   *
   * Example:
   * {{{
   * scala> import cats.std.option._
   * scala> import cats.std.list._
   * scala> import cats.syntax.cartesian._
   * scala> type Error = String
   * scala> val v1: Validated[NonEmptyList[Error], Int] = Validated.Invalid(NonEmptyList("error 1"))
   * scala> val v2: Validated[NonEmptyList[Error], Int] = Validated.Invalid(NonEmptyList("error 2"))
   * scala> val xort: XorT[Option, Error, Int] = XorT(Some(Xor.left("error 3")))
   * scala> xort.withValidated { v3 => (v1 |@| v2 |@| v3.leftMap(NonEmptyList(_))).map{ case (i, j, k) => i + j + k } }
   * res0: XorT[Option, NonEmptyList[Error], Int] = XorT(Some(Left(OneAnd(error 1,List(error 2, error 3)))))
   * }}}
   */
  def withValidated[AA, BB](f: Validated[A, B] => Validated[AA, BB])(implicit F: Functor[F]): XorT[F, AA, BB] =
    XorT(F.map(value)(xor => f(xor.toValidated).toXor))

  def show(implicit show: Show[F[A Xor B]]): String = show.show(value)
}

object XorT extends XorTInstances with XorTFunctions

trait XorTFunctions {
  final def left[F[_], A, B](fa: F[A])(implicit F: Functor[F]): XorT[F, A, B] = XorT(F.map(fa)(Xor.left))

  final def right[F[_], A, B](fb: F[B])(implicit F: Functor[F]): XorT[F, A, B] = XorT(F.map(fb)(Xor.right))

  final def pure[F[_], A, B](b: B)(implicit F: Applicative[F]): XorT[F, A, B] = right(F.pure(b))

  /** Transforms an `Xor` into an `XorT`, lifted into the specified `Applicative`.
   *
   * Note: The return type is a FromXorPartiallyApplied[F], which has an apply method
   * on it, allowing you to call fromXor like this:
   * {{{
   * scala> import cats.std.option._
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
}

private[data] abstract class XorTInstances extends XorTInstances1 {

  /* TODO violates right absorbtion, right distributivity, and left distributivity -- re-enable when MonadCombine laws are split in to weak/strong
  implicit def xorTMonadCombine[F[_], L](implicit F: Monad[F], L: Monoid[L]): MonadCombine[XorT[F, L, ?]] = {
    implicit val F0 = F
    implicit val L0 = L
    new XorTMonadCombine[F, L] { implicit val F = F0; implicit val L = L0 }
  }
  */

  implicit def xorTOrder[F[_], L, R](implicit F: Order[F[L Xor R]]): Order[XorT[F, L, R]] =
    new XorTOrder[F, L, R] {
      val F0: Order[F[L Xor R]] = F
    }

  implicit def xorTShow[F[_], L, R](implicit sh: Show[F[L Xor R]]): Show[XorT[F, L, R]] =
    functor.Contravariant[Show].contramap(sh)(_.value)

  implicit def xorTBifunctor[F[_]](implicit F: Functor[F]): Bifunctor[XorT[F, ?, ?]] =
    new Bifunctor[XorT[F, ?, ?]] {
      override def bimap[A, B, C, D](fab: XorT[F, A, B])(f: A => C, g: B => D): XorT[F, C, D] = fab.bimap(f, g)
    }

  implicit def xorTTraverse[F[_], L](implicit F: Traverse[F]): Traverse[XorT[F, L, ?]] =
    new XorTTraverse[F, L] {
      val F0: Traverse[F] = F
    }

  implicit def xortTransLift[E]: TransLift.Aux[XorT[?[_], E, ?], Functor] =
    new TransLift[XorT[?[_], E, ?]] {
      type TC[M[_]] = Functor[M]

      def liftT[M[_]: Functor, A](ma: M[A]): XorT[M,E,A] =
        XorT(Functor[M].map(ma)(Xor.right))
    }

}

private[data] abstract class XorTInstances1 extends XorTInstances2 {
  /* TODO violates monadFilter right empty law -- re-enable when MonadFilter laws are split in to weak/strong
  implicit def xorTMonadFilter[F[_], L](implicit F: Monad[F], L: Monoid[L]): MonadFilter[XorT[F, L, ?]] = {
    implicit val F0 = F
    implicit val L0 = L
    new XorTMonadFilter[F, L] { implicit val F = F0; implicit val L = L0 }
  }
  */

  implicit def xorTFoldable[F[_], L](implicit F: Foldable[F]): Foldable[XorT[F, L, ?]] =
    new XorTFoldable[F, L] {
      val F0: Foldable[F] = F
    }

  implicit def xorTPartialOrder[F[_], L, R](implicit F: PartialOrder[F[L Xor R]]): PartialOrder[XorT[F, L, R]] =
    new XorTPartialOrder[F, L, R] {
      val F0: PartialOrder[F[L Xor R]] = F
    }

  implicit def xorTBitraverse[F[_]](implicit F: Traverse[F]): Bitraverse[XorT[F, ?, ?]] =
    new XorTBitraverse[F] {
      val F0: Traverse[F] = F
    }
}

private[data] abstract class XorTInstances2 extends XorTInstances3 {
  implicit def xorTMonadError[F[_], L](implicit F: Monad[F]): MonadError[XorT[F, L, ?], L] = {
    implicit val F0 = F
    new XorTMonadError[F, L] { implicit val F = F0 }
  }

  implicit def xorTSemigroupK[F[_], L](implicit F: Monad[F]): SemigroupK[XorT[F, L, ?]] =
    new SemigroupK[XorT[F,L,?]] {
      def combineK[A](x: XorT[F,L,A], y: XorT[F, L, A]): XorT[F, L, A] =
        XorT(F.flatMap(x.value) {
          case l @ Xor.Left(_) => y.value
          case r @ Xor.Right(_) => F.pure(r)
        })
  }

  implicit def xorTEq[F[_], L, R](implicit F: Eq[F[L Xor R]]): Eq[XorT[F, L, R]] =
    new XorTEq[F, L, R] {
      val F0: Eq[F[L Xor R]] = F
    }
}

private[data] abstract class XorTInstances3 {
  implicit def xorTFunctor[F[_], L](implicit F: Functor[F]): Functor[XorT[F, L, ?]] = {
    implicit val F0 = F
    new XorTFunctor[F, L] { implicit val F = F0 }
  }
}

private[data] trait XorTFunctor[F[_], L] extends Functor[XorT[F, L, ?]] {
  implicit val F: Functor[F]
  override def map[A, B](fa: XorT[F, L, A])(f: A => B): XorT[F, L, B] = fa map f
}

private[data] trait XorTMonadError[F[_], L] extends MonadError[XorT[F, L, ?], L] with XorTFunctor[F, L] {
  implicit val F: Monad[F]
  def pure[A](a: A): XorT[F, L, A] = XorT.pure[F, L, A](a)
  def flatMap[A, B](fa: XorT[F, L, A])(f: A => XorT[F, L, B]): XorT[F, L, B] = fa flatMap f
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
  override def attempt[A](fla: XorT[F, L, A]): XorT[F, L, L Xor A] = XorT.right(fla.value)
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
