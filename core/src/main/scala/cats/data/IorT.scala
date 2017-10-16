package cats
package data

final case class IorT[F[_], A, B](value: F[Ior[A, B]]) {

  def fold[C](fa: A => C, fb: B => C, fab: (A, B) => C)(implicit F: Functor[F]): F[C] = F.map(value)(_.fold(fa, fb, fab))

  def isLeft(implicit F: Functor[F]): F[Boolean] = F.map(value)(_.isLeft)

  def isRight(implicit F: Functor[F]): F[Boolean] = F.map(value)(_.isRight)

  def isBoth(implicit F: Functor[F]): F[Boolean] = F.map(value)(_.isBoth)

  def swap(implicit F: Functor[F]): IorT[F, B, A] = IorT(F.map(value)(_.swap))

  def getOrElse[BB >: B](default: => BB)(implicit F: Functor[F]): F[BB] = F.map(value)(_.getOrElse(default))

  def getOrElseF[BB >: B](default: => F[BB])(implicit F: Monad[F]): F[BB] =
    F.flatMap(value) {
      case Ior.Left(_) => default
      case Ior.Right(b) => F.pure(b)
      case Ior.Both(_, b) => F.pure(b)
    }

  def valueOr[BB >: B](f: A => BB)(implicit F: Functor[F], BB: Semigroup[BB]): F[BB] = F.map(value)(_.valueOr(f))

  def forall(f: B => Boolean)(implicit F: Functor[F]): F[Boolean] = F.map(value)(_.forall(f))

  def exists(f: B => Boolean)(implicit F: Functor[F]): F[Boolean] = F.map(value)(_.exists(f))

  def toOption(implicit F: Functor[F]): OptionT[F, B] = OptionT(F.map(value)(_.toOption))

  def toEither(implicit F: Functor[F]): EitherT[F, A, B] = EitherT(F.map(value)(_.toEither))

  def toNested: Nested[F, Ior[A, ?], B] = Nested[F, Ior[A, ?], B](value)

  def merge[AA >: A](implicit ev: B <:< AA, F: Functor[F], AA: Semigroup[AA]): F[AA] = F.map(value)(_.merge(ev, AA))

  def show(implicit show: Show[F[Ior[A, B]]]): String = show.show(value)
}
