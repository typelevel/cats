package cats
package data

/**
 * `OptionT[F[_], A` is a light wrapper on an `F[Option[A]]` with some
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

  def ap[B](f: OptionT[F,A => B])(implicit F: Applicative[F]): OptionT[F,B] = 
    OptionT(F.ap(value)(F.map(f.value)(ff => (aa: Option[A]) => aa match {
                            case Some(a) => ff.map(_(a))
                            case None => None
                          })))

  def flatMap[B](f: A => OptionT[F, B])(implicit F: Monad[F]): OptionT[F, B] =
    OptionT(
      F.flatMap(value){
        case Some(a) => f(a).value
        case None => F.pure(None)
      })

  def flatMapF[B](f: A => F[Option[B]])(implicit F: Monad[F]): OptionT[F, B] =
    OptionT(
      F.flatMap(value){
        case Some(a) => f(a)
        case None => F.pure(None)
      })

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

  def filterNot(p: A => Boolean)(implicit F: Functor[F]): OptionT[F, A] =
    OptionT(F.map(value)(_.filterNot(p)))

  def forall(f: A => Boolean)(implicit F: Functor[F]): F[Boolean] =
    F.map(value)(_.forall(f))

  def isDefined(implicit F: Functor[F]): F[Boolean] =
    F.map(value)(_.isDefined)

  def isEmpty(implicit F: Functor[F]): F[Boolean] =
    F.map(value)(_.isEmpty)

  def orElse(default: => OptionT[F, A])(implicit F: Monad[F]): OptionT[F, A] =
    OptionT(
      F.flatMap(value) {
        case s @ Some(_) => F.pure(s)
        case None => default.value
      })

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
}

object OptionT extends OptionTInstances {
  def pure[F[_], A](a: A)(implicit F: Applicative[F]): OptionT[F, A] =
    OptionT(F.pure(Some(a)))

  /**
   * Transforms an `Option` into an `OptionT`, lifted into the specified `Applicative`.
   *
   * Note: The return type is a FromOptionAux[F], which has an apply method on it, allowing
   * you to call fromOption like this:
   * {{{
   * val t: Option[Int] = ...
   * val x: OptionT[List, Int] = fromOption[List](t)
   * }}}
   *
   * The reason for the indirection is to emulate currying type parameters.
   */
  def fromOption[F[_]]: FromOptionAux[F] = new FromOptionAux

  class FromOptionAux[F[_]] private[OptionT] {
    def apply[A](value: Option[A])(implicit F: Applicative[F]): OptionT[F, A] =
      OptionT(F.pure(value))
  }
}

trait OptionTInstances2 {
  implicit def optionTFunctor[F[_]:Functor]: Functor[OptionT[F, ?]] =
    new Functor[OptionT[F, ?]] {
      override def map[A, B](fa: OptionT[F, A])(f: A => B): OptionT[F, B] =
        fa.map(f)
    }
}

trait OptionTInstances1 extends OptionTInstances2 {
  implicit def optionTApplicative[F[_]:Applicative]: Applicative[OptionT[F, ?]] =
    new Applicative[OptionT[F, ?]] {
      def pure[A](a: A): OptionT[F, A] = OptionT.pure(a)

      override def map[A, B](fa: OptionT[F, A])(f: A => B): OptionT[F, B] =
        fa.map(f)

      override def ap[A,B](fa: OptionT[F, A])(f: OptionT[F, A => B]): OptionT[F, B] =
        fa.ap(f)
    }
}

trait OptionTInstances extends OptionTInstances1 {
  implicit def optionTMonadCombine[F[_]](implicit F: Monad[F]): MonadCombine[OptionT[F, ?]] =
    new MonadCombine[OptionT[F, ?]] {
      def pure[A](a: A): OptionT[F, A] = OptionT.pure(a)

      def flatMap[A, B](fa: OptionT[F, A])(f: A => OptionT[F, B]): OptionT[F, B] =
        fa.flatMap(f)

      override def map[A, B](fa: OptionT[F, A])(f: A => B): OptionT[F, B] =
        fa.map(f)

      override def empty[A]: OptionT[F,A] = OptionT(F.pure(None))
      override def combine[A](x: OptionT[F,A], y: OptionT[F,A]): OptionT[F,A] = x orElse y
    }
  implicit def optionTEq[F[_], A](implicit FA: Eq[F[Option[A]]]): Eq[OptionT[F, A]] =
    FA.on(_.value)
}
