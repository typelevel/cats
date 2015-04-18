package cats
package data

/**
 * Transformer for `Xor`, allowing the effect of an arbitrary type constructor `F` to be combined with the
 * fail-fast effect of `Xor`.
 *
 * `XorT[F, A, B]` wraps a value of type `F[A Xor B]`. An `F[C]` can be lifted in to `XorT[F, A, C]` via `XorT.right`,
 * and lifted in to a `XorT[F, C, B]` via `XorT.left`.
 */
case class XorT[F[_], A, B](value: F[A Xor B]) {

  def fold[C](fa: A => C, fb: B => C)(implicit F: Functor[F]): F[C] = F.map(value)(_.fold(fa, fb))

  def isLeft(implicit F: Functor[F]): F[Boolean] = F.map(value)(_.isLeft)

  def isRight(implicit F: Functor[F]): F[Boolean] = F.map(value)(_.isRight)

  def swap(implicit F: Functor[F]): XorT[F, B, A] = XorT(F.map(value)(_.swap))

  def getOrElse[BB >: B](default: => BB)(implicit F: Functor[F]): F[BB] = F.map(value)(_.getOrElse(default))

  def forall(f: B => Boolean)(implicit F: Functor[F]): F[Boolean] = F.map(value)(_.forall(f))

  def exists(f: B => Boolean)(implicit F: Functor[F]): F[Boolean] = F.map(value)(_.exists(f))

  def toEither(implicit F: Functor[F]): F[Either[A, B]] = F.map(value)(_.toEither)

  def toOption(implicit F: Functor[F]): F[Option[B]] = F.map(value)(_.toOption)

  def to[G[_]](implicit functorF: Functor[F], monoidKG: MonoidK[G], applicativeG: Applicative[G]): F[G[B]] =
    functorF.map(value)(_.to[G, B])

  def collectRight(implicit F: MonadCombine[F]): F[B] =
    F.flatMap(value)(_.to[F, B])

  def bimap[C, D](fa: A => C, fb: B => D)(implicit F: Functor[F]): XorT[F, C, D] = XorT(F.map(value)(_.bimap(fa, fb)))

  def applyAlt[D](ff: XorT[F, A, B => D])(implicit F: Apply[F]): XorT[F, A, D] =
    XorT[F, A, D](F.map2(this.value, ff.value)((xb, xbd) => Apply[A Xor ?].apply(xb)(xbd)))

  def flatMap[AA >: A, D](f: B => XorT[F, AA, D])(implicit F: Monad[F]): XorT[F, AA, D] =
    XorT(F.flatMap(value) {
      case l @ Xor.Left(_) => F.pure(l)
      case Xor.Right(b) => f(b).value
    })

  def flatMapF[AA >: A, D](f: B => F[AA Xor D])(implicit F: Monad[F]): XorT[F, AA, D] =
    flatMap(f andThen XorT.apply)

  def map[D](f: B => D)(implicit F: Functor[F]): XorT[F, A, D] = bimap(identity, f)

  def leftMap[C](f: A => C)(implicit F: Functor[F]): XorT[F, C, B] = bimap(f, identity)

  def compare(that: XorT[F, A, B])(implicit o: Order[F[A Xor B]]): Int =
    o.compare(value, that.value)

  def partialCompare(that: XorT[F, A, B])(implicit p: PartialOrder[F[A Xor B]]): Double =
    p.partialCompare(value, that.value)

  def ===(that: XorT[F, A, B])(implicit eq: Eq[F[A Xor B]]): Boolean =
    eq.eqv(value, that.value)

  def traverse[G[_], D](f: B => G[D])(implicit traverseF: Traverse[F], traverseXorA: Traverse[A Xor ?], applicativeG: Applicative[G]): G[XorT[F, A, D]] =
    applicativeG.map(traverseF.traverse(value)(axb => traverseXorA.traverse(axb)(f)))(XorT.apply)

  def foldLeft[C](c: C)(f: (C, B) => C)(implicit F: Foldable[F]): C =
    F.foldLeft(value, c)((c, axb) => axb.fold(_ => c, f(c, _)))

  def foldRight[C](c: C)(f: (B, C) => C)(implicit F: Foldable[F]): C =
    F.foldRight(value, c)((axb, c) => axb.fold(_ => c, f(_, c)))

  def partialFold[C](f: B => Fold[C])(implicit F: Foldable[F]): Fold[C] =
    F.partialFold(value)(axb => axb.fold(_ => Fold.Pass, f))

  def merge[AA >: A](implicit ev: B <:< AA, F: Functor[F]): F[AA] = F.map(value)(_.fold(identity, ev.apply))

  def combine(that: XorT[F, A, B])(implicit F: Apply[F], A: Semigroup[A], B: Semigroup[B]): XorT[F, A, B] =
    XorT(F.map2(this.value, that.value)(_ combine _))

  def show(implicit show: Show[F[A Xor B]]): String = show.show(value)
}

object XorT extends XorTInstances with XorTFunctions

trait XorTFunctions {

  final def left[F[_], A, B](fa: F[A])(implicit F: Functor[F]): XorT[F, A, B] = XorT(F.map(fa)(Xor.left))

  final def right[F[_], A, B](fb: F[B])(implicit F: Functor[F]): XorT[F, A, B] = XorT(F.map(fb)(Xor.right))

  final def pure[F[_], A, B](b: B)(implicit F: Applicative[F]): XorT[F, A, B] = right(F.pure(b))
}

abstract class XorTInstances extends XorTInstances1 {
  implicit def xorTMonadCombine[F[_], L](implicit F: Monad[F], L: Monoid[L]): MonadCombine[XorT[F, L, ?]] = {
    implicit val F0 = F
    implicit val L0 = L
    new XorTMonadCombine[F, L] { implicit val F = F0; implicit val L = L0 }
  }

  implicit def xorTEq[F[_], L, R](implicit e: Eq[F[L Xor R]]): Eq[XorT[F, L, R]] =
    // TODO Use Eq.instance on next algebra upgrade
    new Eq[XorT[F, L, R]] {
      def eqv(x: XorT[F, L, R], y: XorT[F, L, R]) = e.eqv(x.value, y.value)
    }

  implicit def xorTShow[F[_], L, R](implicit sh: Show[F[L Xor R]]): Show[XorT[F, L, R]] =
    functor.Contravariant[Show].contramap(sh)(_.value)
}

private[data] abstract class XorTInstances1 extends XorTInstances2 {
  implicit def xorTMonadFilter[F[_], L](implicit F: Monad[F], L: Monoid[L]): MonadFilter[XorT[F, L, ?]] = {
    implicit val F0 = F
    implicit val L0 = L
    new XorTMonadFilter[F, L] { implicit val F = F0; implicit val L = L0 }
  }
}

private[data] abstract class XorTInstances2 extends XorTInstances3 {
  implicit def xorTMonad[F[_], L](implicit F: Monad[F]): Monad[XorT[F, L, ?]] = {
    implicit val F0 = F
    new XorTMonad[F, L] { implicit val F = F0 }
  }

  implicit def xorTSemigroupK[F[_], L](implicit F: Monad[F], L: Semigroup[L]): SemigroupK[XorT[F, L, ?]] = {
    implicit val F0 = F
    implicit val L0 = L
    new XorTSemigroupK[F, L] { implicit val F = F0; implicit val L = L0 }
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

private[data] trait XorTMonad[F[_], L] extends Monad[XorT[F, L, ?]] with XorTFunctor[F, L] {
  implicit val F: Monad[F]
  def pure[A](a: A): XorT[F, L, A] = XorT.pure[F, L, A](a)
  def flatMap[A, B](fa: XorT[F, L, A])(f: A => XorT[F, L, B]): XorT[F, L, B] = fa flatMap f
}

private[data] trait XorTSemigroupK[F[_], L] extends SemigroupK[XorT[F, L, ?]] {
  implicit val F: Monad[F]
  implicit val L: Semigroup[L]
  def combine[A](x: XorT[F, L, A], y: XorT[F, L, A]): XorT[F, L, A] =
    XorT(F.flatMap(x.value) {
      case Xor.Left(l1) => F.map(y.value) {
        case Xor.Left(l2) => Xor.Left(L.combine(l1, l2))
        case r @ Xor.Right(_) => r
      }
      case r @ Xor.Right(_) => F.pure[L Xor A](r)
    })
}

private[data] trait XorTMonadFilter[F[_], L] extends MonadFilter[XorT[F, L, ?]] with XorTMonad[F, L] {
  implicit val F: Monad[F]
  implicit val L: Monoid[L]
  def empty[A]: XorT[F, L, A] = XorT(F.pure(Xor.left(L.empty)))
}

private[data] trait XorTMonadCombine[F[_], L] extends MonadCombine[XorT[F, L, ?]] with XorTMonadFilter[F, L] with XorTSemigroupK[F, L] {
  implicit val F: Monad[F]
  implicit val L: Monoid[L]
}


