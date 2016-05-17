package cats
package data

/**
 * `OptionT[F[_], A]` is a light wrapper on an `F[Option[A]]` with some
 * convenient methods for working with this nested structure.
 *
 * It may also be said that `OptionT` is a monad transformer for `Option`.
 *
 * For more information, see the [[http://non.github.io/cats/tut/optiont.html documentation]].
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

  def flatMap[B](f: A => OptionT[F, B])(implicit F: Monad[F]): OptionT[F, B] =
    flatMapF(a => f(a).value)

  def flatMapF[B](f: A => F[Option[B]])(implicit F: Monad[F]): OptionT[F, B] =
    OptionT(
      F.flatMap(value){
        case Some(a) => f(a)
        case None => F.pure(None)
      })

  def transform[B](f: Option[A] => Option[B])(implicit F: Functor[F]): OptionT[F, B] =
    OptionT(F.map(value)(f))

  def subflatMap[B](f: A => Option[B])(implicit F: Functor[F]): OptionT[F, B] =
    transform(_.flatMap(f))

  def getOrElse(default: => A)(implicit F: Functor[F]): F[A] =
    F.map(value)(_.getOrElse(default))

  def getOrElseF(default: => F[A])(implicit F: Monad[F]): F[A] =
    F.flatMap(value){
      case Some(a) => F.pure(a)
      case None => default
    }

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
   * Note: The return type is a FromOptionPartiallyApplied[F], which has an apply method
   * on it, allowing you to call fromOption like this:
   * {{{
   * scala> import cats.std.list._
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
    *
    */
  def liftF[F[_], A](fa: F[A])(implicit F: Functor[F]): OptionT[F, A] = OptionT(F.map(fa)(Some(_)))
}

private[data] sealed trait OptionTInstances2 {
  implicit def catsDataFunctorForOptionT[F[_]:Functor]: Functor[OptionT[F, ?]] =
    new Functor[OptionT[F, ?]] {
      override def map[A, B](fa: OptionT[F, A])(f: A => B): OptionT[F, B] =
        fa.map(f)
    }

  // do NOT change this to val! I know it looks like it should work, and really I agree, but it doesn't (for... reasons)
  implicit def catsDataTransLiftForOptionT: TransLift.Aux[OptionT, Functor] =
    new TransLift[OptionT] {
      type TC[M[_]] = Functor[M]

      def liftT[M[_]: Functor, A](ma: M[A]): OptionT[M, A] = OptionT.liftF(ma)
    }
}

private[data] sealed trait OptionTInstances1 extends OptionTInstances2 {

  implicit def catsDataMonadForOptionT[F[_]](implicit F0: Monad[F]): Monad[OptionT[F, ?]] =
    new OptionTMonad[F] { implicit val F = F0 }
}

private[data] sealed trait OptionTInstances extends OptionTInstances1 {
  implicit def catsDataMonadRecForOptionT[F[_]](implicit F0: MonadRec[F]): MonadRec[OptionT[F, ?]] =
    new OptionTMonadRec[F] { implicit val F = F0 }

  implicit def catsDataEqForOptionT[F[_], A](implicit FA: Eq[F[Option[A]]]): Eq[OptionT[F, A]] =
    FA.on(_.value)

  implicit def catsDataShowForOptionT[F[_], A](implicit F: Show[F[Option[A]]]): Show[OptionT[F, A]] =
    functor.Contravariant[Show].contramap(F)(_.value)
}

private[data] trait OptionTMonad[F[_]] extends Monad[OptionT[F, ?]] {
  implicit val F: Monad[F]

  def pure[A](a: A): OptionT[F, A] = OptionT.pure(a)

  def flatMap[A, B](fa: OptionT[F, A])(f: A => OptionT[F, B]): OptionT[F, B] =
    fa.flatMap(f)

  override def map[A, B](fa: OptionT[F, A])(f: A => B): OptionT[F, B] =
    fa.map(f)
}

private[data] trait OptionTMonadRec[F[_]] extends MonadRec[OptionT[F, ?]] with OptionTMonad[F] {
  implicit val F: MonadRec[F]

  def tailRecM[A, B](a: A)(f: A => OptionT[F, A Xor B]): OptionT[F, B] =
    OptionT(F.tailRecM(a)(a0 => F.map(f(a0).value){
      case None => Xor.Right(None)
      case Some(Xor.Left(a1)) => Xor.Left(a1)
      case Some(Xor.Right(b)) => Xor.Right(Some(b))
    }))
}
