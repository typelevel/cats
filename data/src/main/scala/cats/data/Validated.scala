package cats
package data

import cats.data.Or.{LeftOr, RightOr}
import cats.data.Validated.{Valid, Invalid}

sealed abstract class Validated[+E, +A] extends Serializable {
  def fold[B](fe: E => B, fa: A => B): B = this match {
    case Invalid(e) => fe(e)
    case Valid(a) => fa(a)
  }

  def isValid: Boolean = fold(_ => false, _ => true)
  def isInvalid: Boolean = fold(_ => true, _ => false)

  /**
   * Run the side-effecting function on the value if it is Valid
   */
  def foreach(f: A => Unit): Unit = fold(_ => (), f)

  /**
   * Return the Valid value, or the default if Invalid
   */
  def getOrElse[B >: A](default: => B): B = fold(_ => default, identity)

  /**
   * Is the value Valid and matching the given predicate
   */
  def exists(predicate: A => Boolean): Boolean = fold(_ => false, predicate)

  /**
   * If the value is Valid but the predicate fails, return an empty
   * Invalid value, otherwise leaves the value unchanged.  This method
   * is mostly useful for allowing validated values to be used in a
   * for comprehension with pattern matching.
   */
  def filter[EE >: E](pred: A => Boolean)(implicit M: Monoid[EE]): Validated[EE,A] =
    fold(Invalid.apply, a => if(pred(a)) this else Invalid(M.empty))

  /**
   * Converts the value to an Either[E,A]
   */
  def toEither = fold(Left.apply, Right.apply)

  /**
   * Returns Valid values wrapped in Some, and None for Invalid values
   */
  def toOption: Option[A] = fold(_ => None, Some.apply)

  /**
   * Convert this value to a single element List if it is Valid,
   * otherwise return an empty List
   */
  def toList: List[A] = fold(_ => Nil, List(_))

  /**
   * Convert this value to RightOr if Valid or LeftOr if Invalid
   */
  def toOr: Or[E,A] = fold(LeftOr.apply,RightOr.apply)

  /**
   * Validated is a [[Bifunctor]], this method applies one of the
   * given functions.
   */
  def bimap[EE, AA](fe: E => EE, fa: A => AA): Validated[EE, AA] =
    fold(fe andThen Invalid.apply,
         fa andThen Valid.apply)

  /**
   * 
   */
  def apply[EE >: E, B](f: Validated[EE, A => B])(implicit EE: Semigroup[EE]): Validated[EE,B] =
    (this, f) match {
      case (Valid(a), Valid(f)) => Valid(f(a))
      case (Invalid(e1), Invalid(e2)) => Invalid(EE.combine(e1,e2))
      case (e @ Invalid(_), _) => e
      case (_, e @ Invalid(_)) => e

    }

  

  /**
   * A monadic bind which applies a function if the value is Valid.
   */
  def flatMap[EE >: E, B](f: A => Validated[EE,B]): Validated[EE,B] =
    fold(Invalid.apply, f)

  /**
   * Apply a function to a Valid value, returning a new Valid value
   */
  def map[B](f: A => B): Validated[E,B] = bimap(identity, f)

  /**
   * When Valid, apply the function, marking the result as valid
   * inside the Applicative's context,
   * when Invalid, lift the Error into the Applicative's contexst
   */
  def traverse[F[_], EE >: E, B](f: A => F[B])(implicit F: Applicative[F]): F[Validated[EE,B]] =
    fold(e => F.pure(Invalid(e)),
         a => F.map(f(a))(Valid.apply))

  /**
   * apply the given function to the value with the given B when
   * valid, otherwise return the given B
   */
  def foldLeft[B](b: B)(f: (B, A) => B): B = fold(_ => b, f(b, _))

  /**
   * apply the given function to the value with the given B when
   * valid, otherwise return the given B
   */
  def foldRight[B](b: B)(f: (A, B) => B): B = fold(_ => b, f(_, b))

  /**
   * lazily apply the given function to the value with the given B when
   * valid, otherwise return the given B
   */
  def foldLazy[B](b: Lazy[B])(f: A => Fold[B]): Lazy[B] =
    fold(_ => b, a => Lazy(f(a).complete(b.value)))

  def show[EE >: E, AA >: A](implicit EE: Show[EE], AA: Show[AA]): String =
    fold(e => s"Invalid(${EE.show(e)})",
         a => s"Valid(${AA.show(a)})")
}

object Validated extends ValidatedInstances {
  final case class Valid[+A](a: A) extends Validated[Nothing, A]
  final case class Invalid[+E](e: E) extends Validated[E, Nothing]
}

sealed abstract class ValidatedInstances {
  def validatedShow[E,A](implicit E: Show[E], A: Show[A]): Show[Validated[E,A]] = new Show[Validated[E,A]] {
    def show(f: Validated[E,A]) = f.show
  }

  def validatedInstances[E]: Monad[Validated[E,?]] = new Monad[Validated[E,?]] {
    override def map[A,B](fa: Validated[E,A])(f: A => B) = fa map f
    override def flatMap[A,B](fa: Validated[E,A])(f: A => Validated[E,B]) = fa flatMap f
    override def pure[A](a: A): Validated[Nothing, A] = Valid(a)
  }
}
