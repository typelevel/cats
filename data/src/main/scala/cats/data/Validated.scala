package cats
package data

import cats.data.Validated.{Valid, Invalid}

sealed abstract class Validated[+E, +A] extends Product with Serializable {

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
   * Is this Valid and matching the given predicate
   */
  def exists(predicate: A => Boolean): Boolean = fold(_ => false, predicate)

  /**
   * Is this Invalid or matching the predicate
   */
  def forAll(f: A => Boolean): Boolean = fold(_ => true, f)

  /**
   * If the value is Valid but the predicate fails, return an empty
   * Invalid value, otherwise leaves the value unchanged.  This method
   * is mostly useful for allowing validated values to be used in a
   * for comprehension with pattern matching.
   */
  def filter[EE >: E](pred: A => Boolean)(implicit M: Monoid[EE]): Validated[EE,A] =
    fold(Invalid.apply, a => if(pred(a)) this else Invalid(M.empty))

  /**
   * Return this if it is Valid, or else fall back to the given default.
   */
  def orElse[EE >: E, AA >: A](default: => Validated[EE,AA]): Validated[EE,AA] =
    fold(_ => default, _ => this)

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
  def toXor: Xor[E,A] = fold(Xor.Left.apply,Xor.Right.apply)

  /**
   * Convert to an Xor, apply a function, convert back.  This is handy
   * when you want to use the Monadic properties of the Xor type.
   */
  def withXor[EE,B](f: (E Xor A) => (EE Xor B)): Validated[EE,B] =
    f(toXor).toValidated

  /**
   * Validated is a [[functor.Bifunctor]], this method applies one of the
   * given functions.
   */
  def bimap[EE, AA](fe: E => EE, fa: A => AA): Validated[EE, AA] =
    fold(fe andThen Invalid.apply,
         fa andThen Valid.apply)

  /**
   * From Apply:
   * if both the function and this value are Valid, apply the function
   */
  def apply[EE >: E, B](f: Validated[EE, A => B])(implicit EE: Semigroup[EE]): Validated[EE,B] =
    (this, f) match {
      case (Valid(a), Valid(f)) => Valid(f(a))
      case (Invalid(e1), Invalid(e2)) => Invalid(EE.combine(e1,e2))
      case (e @ Invalid(_), _) => e
      case (_, e @ Invalid(_)) => e
    }

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

  def partialFold[B](f: A => Fold[B]): Fold[B] =
    fold(_ => Fold.Pass, f)

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

  def validatedInstances[E](E : Semigroup[E]): Applicative[Validated[E,?]] = new Applicative[Validated[E,?]] {
    override def map[A,B](fa: Validated[E,A])(f: A => B) = fa map f

    override def apply[A,B](fa: Validated[E,A])(f: Validated[E,A=>B]) =
      (fa,f) match {
        case (Valid(a),Valid(f)) => Valid(f(a))
        case (e @ Invalid(_), Valid(_)) => e
        case (Valid(_), e @ Invalid(_)) => e
        case (Invalid(e1), Invalid(e2)) => Invalid(E.combine(e1, e2))
      }

    override def pure[A](a: A): Validated[Nothing, A] = Valid(a)
  }
}
