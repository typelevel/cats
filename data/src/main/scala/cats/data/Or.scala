package cats
package data

import algebra.{Eq, Monoid, Order, Semigroup}

import scala.util.control.NonFatal

/* Represents a disjunction: a result that is either an `A` or a `B`.
 *
 * An instance of `A` [[Or]] B is either a [[LOr]]`[A]` (aka a "left") or a [[ROr]]`[B]` (aka a "right").
 *
 * A common use of a disjunction is to explicitly represent the possibility of failure in a result as opposed to
 * throwing an exception. By convention, the left is used for errors and the right is reserved for successes.
 * For example, a function that attempts to parse an integer from a string may have a return type of
 * `NumberFormatException` [[Or]] `Int`. However, since there is no need to actually throw an exception, the type (`A`)
 * chosen for the "left" could be any type representing an error and has no need to actually extend `Exception`.
 *
 * `A` [[Or]] `B` is isomorphic to `scala.Either[A, B]`, but [[Or]] is right-biased, so methods such as `map` and
 * `flatMap` apply only in the context of the "right" case. This right bias makes [[Or]] more convenient to use
 * than `scala.Either` in a monadic context. Methods such as `swap`, `swapped`, and `leftMap` provide functionality
 * that `scala.Either` exposes through left projections.
 */
sealed abstract class Or[+A, +B] extends Product with Serializable {
  /*
  final class SwitchingOr[X](r: => X) {
    def <<?:(left: => X): X =
      Or.this match {
        case LOr(_) => left
        case ROr(_) => r
      }
  }

  /** If this disjunction is right, return the given X value, otherwise, return the X value given to the return value. */
  def :?>>[X](right: => X): SwitchingOr[X] =
    new SwitchingOr[X](right)
  */

  /** Return `true` if this disjunction is left. */
  def isLeft: Boolean =
    this match {
      case LOr(_) => true
      case ROr(_) => false
    }

  /** Return `true` if this disjunction is right. */
  def isRight: Boolean =
    this match {
      case LOr(_) => false
      case ROr(_) => true
    }

  /** Catamorphism. Run the first given function if left, otherwise, the second given function. */
  def fold[X](l: A => X, r: B => X): X =
    this match {
      case LOr(a) => l(a)
      case ROr(b) => r(b)
    }

  /** Spin in tail-position on the right value of this disjunction. */
  def loopr[AA >: A, BB >: B, X](left: AA => X, right: BB => X Or (AA Or BB)): X =
    Or.loopRight(this, left, right)

  /** Spin in tail-position on the left value of this disjunction. */
  def loopl[AA >: A, BB >: B, X](left: AA => X Or (AA Or BB), right: BB => X): X =
    Or.loopLeft(this, left, right)

  /** Flip the left/right values in this disjunction. Alias for `unary_~` */
  def swap: (B Or A) =
    this match {
      case LOr(a) => ROr(a)
      case ROr(b) => LOr(b)
    }

  /** Flip the left/right values in this disjunction. Alias for `swap` */
  def unary_~ : (B Or A) =
    swap

  /** Run the given function on this swapped value. Alias for `~` */
  def swapped[AA, BB](k: (B Or A) => (BB Or AA)): (AA Or BB) =
    k(swap).swap

  /** Run the given function on this swapped value. Alias for `swapped` */
  def ~[AA, BB](k: (B Or A) => (BB Or AA)): (AA Or BB) =
    swapped(k)

  /** Binary functor map on this disjunction. */
  def bimap[C, D](f: A => C, g: B => D): (C Or D) =
    this match {
      case LOr(a) => LOr(f(a))
      case ROr(b) => ROr(g(b))
    }

  /** Run the given function on the left value. */
  def leftMap[C](f: A => C): (C Or B) =
    this match {
      case LOr(a) => LOr(f(a))
      case b @ ROr(_) => b
    }

  /** Binary functor traverse on this disjunction. */
  def bitraverse[F[_]: Functor, C, D](f: A => F[C], g: B => F[D]): F[C Or D] =
    this match {
      case LOr(a) => Functor[F].map(f(a))(LOr(_))
      case ROr(b) => Functor[F].map(g(b))(ROr(_))
    }

  /** Map on the right of this disjunction. */
  def map[D](g: B => D): (A Or D) =
    this match {
      case ROr(a)     => ROr(g(a))
      case b @ LOr(_) => b
    }

  /** Traverse on the right of this disjunction. */
  def traverse[F[_]: Applicative, AA >: A, D](g: B => F[D]): F[AA Or D] =
    this match {
      case a @ LOr(_) => Applicative[F].pure(a)
      case ROr(b) => Functor[F].map(g(b))(ROr(_))
    }

  /** Run the side-effect on the right of this disjunction. */
  def foreach(g: B => Unit): Unit = {
    bimap(_ => (), g)
    ()
  }

  /** Apply a function in the environment of the right of this disjunction. */
  def ap[AA >: A, C](f: => AA Or (B => C)): (AA Or C) =
    f flatMap (ff => map(ff(_)))

  /** Bind through the right of this disjunction. */
  def flatMap[AA >: A, D](g: B => (AA Or D)): (AA Or D) =
    this match {
      case a @ LOr(_) => a
      case ROr(b) => g(b)
    }

  /** Fold on the right of this disjunction. */
  def foldRight[Z](z: => Z)(f: (B, => Z) => Z): Z =
    this match {
      case LOr(_) => z
      case ROr(a) => f(a, z)
    }

  /** Filter on the right of this disjunction. */
  def filter[AA >: A](p: B => Boolean)(implicit M: Monoid[AA]): (AA Or B) =
    this match {
      case LOr(_) => this
      case ROr(b) => if(p(b)) this else LOr(M.empty)
    }

  /** Return `true` if this disjunction is a right value satisfying the given predicate. */
  def exists[BB >: B](p: BB => Boolean): Boolean =
    this match {
      case LOr(_) => false
      case ROr(b) => p(b)
    }

  /** Return `true` if this disjunction is a left value or the right value satisfies the given predicate. */
  def forall[BB >: B](p: BB => Boolean): Boolean =
    this match {
      case LOr(_) => true
      case ROr(b) => p(b)
    }

  /** Return an empty list or list with one element on the right of this disjunction. */
  def toList: List[B] =
    this match {
      case LOr(_) => Nil
      case ROr(b) => b :: Nil
    }

  /** Return an empty stream or stream with one element on the right of this disjunction. */
  def toStream: Stream[B] =
    this match {
      case LOr(_) => Stream()
      case ROr(b) => Stream(b)
    }

  /** Return an empty option or option with one element on the right of this disjunction. Useful to sweep errors under the carpet. */
  def toOption: Option[B] =
    this match {
      case LOr(_) => None
      case ROr(b) => Some(b)
    }

  /*
  /** Return an empty maybe or option with one element on the right of this disjunction. Useful to sweep errors under the carpet. */
  def toMaybe[BB >: B]: Maybe[BB] =
    this match {
      case LOr(_) => Maybe.empty
      case ROr(b) => Maybe.just(b)
    }
  */

  /** Convert to a core `scala.Either` at your own peril. */
  def toEither: Either[A, B] =
    this match {
      case LOr(a) => Left(a)
      case ROr(b) => Right(b)
    }

  /** Return the right value of this disjunction or the given default if left. Alias for `|` */
  def getOrElse[BB >: B](x: => BB): BB =
    this match {
      case LOr(_) => x
      case ROr(b) => b
    }

  /** Return the right value of this disjunction or the given default if left. Alias for `getOrElse` */
  def |[BB >: B](x: => BB): BB =
    getOrElse(x)

  /** Return the right value of this disjunction or run the given function on the left. */
  def valueOr[BB >: B](x: A => BB): BB =
    this match {
      case LOr(a) => x(a)
      case ROr(b) => b
    }

  /** Return this if it is a right, otherwise, return the given value. Alias for `|||` */
  def orElse[AA >: A, BB >: B](x: => AA Or BB): AA Or BB =
    this match {
      case LOr(_) => x
      case ROr(_) => this
    }

  /** Return this if it is a right, otherwise, return the given value. Alias for `orElse` */
  def |||[AA >: A, BB >: B](x: => AA Or BB): AA Or BB =
    orElse(x)

  /**
   * Sums up values inside disjunction, if both are left or right. Returns first left otherwise.
   * {{{
   * ROr(v1) +++ ROr(v2) → ROr(v1 + v2)
   * ROr(v1) +++ LOr(v2) → LOr(v2)
   * LOr(v1) +++ ROr(v2) → LOr(v1)
   * LOr(v1) +++ LOr(v2) → LOr(v1 + v2)
   * }}}
   */
  def +++[AA >: A, BB >: B](x: => AA Or BB)(implicit M1: Semigroup[BB], M2: Semigroup[AA]): AA Or BB =
    this match {
      case LOr(a1) => x match {
        case LOr(a2) => LOr(M2.combine(a1, a2))
        case ROr(_) => this
      }
      case ROr(b1) => x match {
        case b2 @ LOr(_) => b2
        case ROr(b2) => ROr(M1.combine(b1, b2))
      }
    }

  /** Ensures that the right value of this disjunction satisfies the given predicate, or returns left with the given value. */
  def ensure[AA >: A](onLeft: => AA)(f: B => Boolean): (AA Or B) = this match {
    case ROr(b) => if (f(b)) this else LOr(onLeft)
    case LOr(_) => this
  }

  /** Compare two disjunction values for equality. */
  def ===[AA >: A, BB >: B](x: AA Or BB)(implicit EA: Eq[AA], EB: Eq[BB]): Boolean =
    this match {
      case LOr(a1) => x match {
        case LOr(a2) => Eq[AA].eqv(a1, a2)
        case ROr(_) => false
      }
      case ROr(b1) => x match {
        case ROr(b2) => Eq[BB].eqv(b1, b2)
        case LOr(_) => false
      }
    }

  /** Compare two disjunction values for ordering. */
  def compare[AA >: A, BB >: B](x: AA Or BB)(implicit EA: Order[AA], EB: Order[BB]): Int =
    this match {
      case LOr(a1) => x match {
        case LOr(a2) => Order[AA].compare(a1, a2)
        case ROr(_) => -1
      }
      case ROr(b1) => x match {
        case ROr(b2) => Order[BB].compare(b1, b2)
        case LOr(_) => 1
      }
    }

  /*
  /** Convert to a validation. */
  def validation: Validation[A, B] =
    this match {
      case LOr(a) => Failure(a)
      case ROr(b) => Success(b)
    }

  /** Run a validation function and back to disjunction again. Alias for `@\?/` */
  def validationed[AA, BB](k: Validation[A, B] => Validation[AA, BB]): AA Or BB =
    k(validation).disjunction

  /** Run a validation function and back to disjunction again. Alias for `validationed` */
  def @\?/[AA, BB](k: Validation[A, B] => Validation[AA, BB]): AA Or BB =
    validationed(k)

  /** Return the value from whichever side of the disjunction is defined, given a commonly assignable type. */
  def merge[AA >: A](implicit ev: B <~< AA): AA =
    this match {
      case LOr(a) => a
      case ROr(b) => ev(b)
    }

  /** Convert to a These. */
  def toThese: A \&/ B =
    fold(
      a => \&/.This(a),
      b => \&/.That(b)
    )
    */

}

/** A left disjunction
 *
 * Often used to represent the failure case of a result
 */
final case class LOr[+A](a: A) extends (A Or Nothing)

/** A right disjunction
 *
 * Often used to represent the success case of a result
 */
final case class ROr[+B](b: B) extends (Nothing Or B)

object Or extends OrInstances with OrFunctions {

  /** Spin in tail-position on the right value of the given disjunction. */
  @annotation.tailrec
  final def loopRight[A, B, X](d: A Or B, left: A => X, right: B => X Or (A Or B)): X =
    d match {
      case LOr(a) => left(a)
      case ROr(b) => right(b) match {
        case LOr(x) => x
        case ROr(q) => loopRight(q, left, right)
      }
    }

  /** Spin in tail-position on the left value of the given disjunction. */
  @annotation.tailrec
  final def loopLeft[A, B, X](d: A Or B, left: A => X Or (A Or B), right: B => X): X =
    d match {
      case LOr(a) => left(a) match {
        case LOr(x) => x
        case ROr(q) => loopLeft(q, left, right)
      }
      case ROr(b) => right(b)
    }

}

sealed abstract class OrInstances extends OrInstances0 {
  implicit def OrOrder[A: Order, B: Order]: Order[A Or B] =
    new Order[A Or B] {
      def compare(a1: A Or B, a2: A Or B) =
        a1 compare a2
      override def eqv(a1: A Or B, a2: A Or B) =
        a1 === a2
    }

  implicit def OrMonoid[A: Semigroup, B: Monoid]: Monoid[A Or B] =
    new Monoid[A Or B] {
      def combine(a1: A Or B, a2: A Or B) =
        a1 +++ a2
      def empty =
        ROr(Monoid[B].empty)
    }

  implicit def OrShow[A, B](implicit showA: Show[A], showB: Show[B]): Show[A Or B] =
    Show.show[A Or B](_.fold(a => s"LOr(${showA.show(a)})", b => s"ROr(${showB.show(b)})"))

}

sealed abstract class OrInstances0 extends OrInstances1 {
  implicit def OrEqual[A: Eq, B: Eq]: Eq[A Or B] =
    new Eq[A Or B] {
      def eqv(a1: A Or B, a2: A Or B) =
        a1 === a2
    }

  implicit def OrSemigroup[A: Semigroup, B: Semigroup]: Semigroup[A Or B] =
    new Semigroup[A Or B] {
      def combine(a1: A Or B, a2: A Or B) =
        a1 +++ a2
    }
}

sealed abstract class OrInstances1 extends OrInstances2 {
  implicit def OrInstances1[L]: /*Traverse[L Or ?] with */ Monad[L Or ?] /*with Cozip[L Or ?] with Plus[L Or ?] with Optional[L Or ?] with MonadError[Or, L]*/ =
    new /*Traverse[L Or ?] with */ Monad[L Or ?] /*with Cozip[L Or ?] with Plus[L Or ?] with Optional[L Or ?] with Monad[Or, L]*/ {
      override def map[A, B](fa: L Or A)(f: A => B) =
        fa map f

      def flatMap[A, B](fa: L Or A)(f: A => L Or B) =
        fa flatMap f

      def pure[A](a: A) =
        ROr(a)

      def traverseImpl[G[_] : Applicative, A, B](fa: L Or A)(f: A => G[B]) =
        fa.traverse(f)

      /*
      override def foldRight[A, B](fa: L Or A, z: Lazy[B])(f: (A, => B) => B) =
        fa.foldRight(z)(f)
      */

      def cozip[A, B](x: L Or (A Or B)) =
        x match {
          case l @ LOr(_) => LOr(l)
          case ROr(e) => e match {
            case LOr(a) => LOr(ROr(a))
            case b @ ROr(_) => ROr(b)
          }
        }

      def plus[A](a: L Or A, b: => L Or A) =
        a orElse b

      def pextract[B, A](fa: L Or A): (L Or B) Or A = fa match {
        case l@ LOr(_) => LOr(l)
        case r@ ROr(_) => r
      }

      def raiseError[A](e: L): L Or A =
        LOr(e)

      def handleError[A](fa: L Or A)(f: L => L Or A): L Or A = fa match {
        case LOr(e) => f(e)
        case r => r
      }
    }
}

sealed abstract class OrInstances2 {
  /*
  implicit val OrInstances2 : Bitraverse[Or] = new Bitraverse[Or] {
    override def bimap[A, B, C, D](fab: A Or B)
                                  (f: A => C, g: B => D) = fab bimap (f, g)

    def bitraverseImpl[G[_] : Applicative, A, B, C, D](fab: A Or B)
                                                  (f: A => G[C], g: B => G[D]) =
      fab.bitraverse(f, g)
  }

  implicit val OrAssociative: Associative[Or] = new Associative[Or] {
    def reassociateLeft[A, B, C](f: Or[A, Or[B, C]]) =
      f.fold(
        a => Or.left(Or.left(a)),
        _.fold(
          b => Or.left(Or.right(b)),
          Or.right(_)
        )
      )

    def reassociateRight[A, B, C](f: Or[Or[A, B], C]) =
      f.fold(
        _.fold(
          Or.left(_),
          b => Or.right(Or.left(b))
        ),
        c => Or.right(Or.right(c))
      )
  }
  */
}

trait OrFunctions {
  /** Construct a left disjunction value. */
  def left[A, B]: A => A Or B =
    LOr(_)

  /** Construct a right disjunction value. */
  def right[A, B]: B => A Or B =
    ROr(_)

  /** Construct a disjunction value from a standard `scala.Either`. */
  def fromEither[A, B](e: Either[A, B]): A Or B =
    e fold (left, right)

  /*
  def fromTryCatchThrowable[T, E <: Throwable](a: => T)(implicit nn: NotNothing[E], ex: ClassTag[E]): E Or T = try {
    ROr(a)
  } catch {
    case e if ex.runtimeClass.isInstance(e) => LOr(e.asInstanceOf[E])
  }
  */

  def fromTryCatchNonFatal[T](a: => T): Throwable Or T = try {
    ROr(a)
  } catch {
    case NonFatal(t) => LOr(t)
  }
}
