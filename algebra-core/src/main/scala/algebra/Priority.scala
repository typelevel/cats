package algebra

/**
 * Priority is a type class for prioritized implicit search.
 *
 * This type class will attempt to provide an implicit instance of `P`
 * (the preferred type). If that type is not available it will
 * fallback to `F` (the fallback type). If neither type is available
 * then a `Priority[P, F]` instance will not be available.
 *
 * This type can be useful for problems where multiple algorithms can
 * be used, depending on the type classes available.
 */
sealed trait Priority[+P, +F] {

  import Priority.{Fallback, Preferred}

  def fold[B](f1: P => B)(f2: F => B): B =
    this match {
      case Preferred(x) => f1(x)
      case Fallback(y)  => f2(y)
    }

  def join[U >: P with F]: U =
    fold(_.asInstanceOf[U])(_.asInstanceOf[U])

  def bimap[P2, F2](f1: P => P2)(f2: F => F2): Priority[P2, F2] =
    this match {
      case Preferred(x) => Preferred(f1(x))
      case Fallback(y)  => Fallback(f2(y))
    }

  def toEither: Either[P, F] =
    fold[Either[P, F]](p => Left(p))(f => Right(f))

  def isPreferred: Boolean =
    fold(_ => true)(_ => false)

  def isFallback: Boolean =
    fold(_ => false)(_ => true)

  def getPreferred: Option[P] =
    fold[Option[P]](p => Some(p))(_ => None)

  def getFallback: Option[F] =
    fold[Option[F]](_ => None)(f => Some(f))
}

object Priority extends FindPreferred {

  case class Preferred[P](get: P) extends Priority[P, Nothing]
  case class Fallback[F](get: F) extends Priority[Nothing, F]

  def apply[P, F](implicit ev: Priority[P, F]): Priority[P, F] = ev
}

private[algebra] trait FindPreferred extends FindFallback {
  implicit def preferred[P](implicit ev: P): Priority[P, Nothing] =
    Priority.Preferred(ev)
}

private[algebra] trait FindFallback {
  implicit def fallback[F](implicit ev: F): Priority[Nothing, F] =
    Priority.Fallback(ev)
}
