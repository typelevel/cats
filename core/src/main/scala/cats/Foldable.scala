package cats

import scala.collection.mutable
import simulacrum._

/**
 * Data structures that can be folded to a summary value.
 *
 * In the case of a collection (such as `List` or `Set`), these
 * methods will fold together (combine) the values contained in the
 * collection to produce a single result. Most collection types have
 * `foldLeft` methods, which will usually be used by the associationed
 * `Fold[_]` instance.
 *
 * Foldable[F] is implemented in terms of two basic methods:
 *
 *  - `foldLeft(fa, b)(f)` eagerly folds `fa` from left-to-right.
 *  - `foldRight(fa, b)(f)` lazily folds `fa` from right-to-left.
 *
 * (Actually `foldRight` is implemented in terms of `partialFold`.)
 *
 * Beyond these it provides many other useful methods related to
 * folding over F[A] values.
 *
 * See: [[https://www.cs.nott.ac.uk/~gmh/fold.pdf A tutorial on the universality and expressiveness of fold]]
 */
@typeclass trait Foldable[F[_]] extends Serializable { self =>

  /**
   * Left associative fold on 'F' using the function 'f'.
   */
  def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B

  /**
   * Right associative lazy fold on `F` using the folding function 'f'.
   *
   * This method evaluates `b` lazily (in some cases it will not be
   * needed), and returns a lazy value. We are using `A => Fold[B]` to
   * support laziness in a stack-safe way.
   *
   * For more detailed information about how this method works see the
   * documentation for `Fold[_]`.
   */
  def foldRight[A, B](fa: F[A], lb: Lazy[B])(f: A => Fold[B]): Lazy[B] =
    Lazy(partialFold[A, B](fa)(f).complete(lb))

  /**
   * Low-level method that powers `foldRight`.
   */
  def partialFold[A, B](fa: F[A])(f: A => Fold[B]): Fold[B]


  def reduceLeftToOption[A, B](fa: F[A])(f: A => B)(g: (B, A) => B): Option[B] =
    foldLeft(fa, Option.empty[B]) {
      case (Some(b), a) => Some(g(b, a))
      case (None, a) => Some(f(a))
    }

  def reduceRightToOption[A, B](fa: F[A])(f: A => B)(g: A => Fold[B]): Lazy[Option[B]] =
    foldRight(fa, Lazy.eager(Option.empty[B])) { a =>
      Fold.Continue {
        case None => Some(f(a))
        case Some(b) => Some(g(a).complete(Lazy.eager(f(a))))
      }
    }

  /**
   * Fold implemented using the given Monoid[A] instance.
   */
  def fold[A](fa: F[A])(implicit A: Monoid[A]): A =
    foldLeft(fa, A.empty) { (acc, a) =>
      A.combine(acc, a)
    }

  /**
   * Fold implemented by mapping `A` values into `B` and then
   * combining them using the given `Monoid[B]` instance.
   */
  def foldMap[A, B](fa: F[A])(f: A => B)(implicit B: Monoid[B]): B =
    foldLeft(fa, B.empty)((b, a) => B.combine(b, f(a)))

  /**
   * Traverse `F[A]` using `Applicative[G]`.
   *
   * `A` values will be mapped into `G[B]` and combined using
   * `Applicative#map2`.
   *
   * For example:
   *
   * {{{
   *     def parseInt(s: String): Option[Int] = ...
   *     val F = Foldable[List]
   *     F.traverse_(List("333", "444"))(parseInt) // Some(())
   *     F.traverse_(List("333", "zzz"))(parseInt) // None
   * }}}
   *
   * This method is primarily useful when `G[_]` represents an action
   * or effect, and the specific `A` aspect of `G[A]` is not otherwise
   * needed.
   */
  def traverse_[G[_], A, B](fa: F[A])(f: A => G[B])(implicit G: Applicative[G]): G[Unit] =
    foldLeft(fa, G.pure(())) { (acc, a) =>
      G.map2(acc, f(a)) { (_, _) => () }
    }

  /**
   * Sequence `F[G[A]]` using `Applicative[G]`.
   *
   * This is similar to `traverse_` except it operates on `F[G[A]]`
   * values, so no additional functions are needed.
   *
   * For example:
   *
   * {{{
   *     val F = Foldable[List]
   *     F.sequence_(List(Option(1), Option(2), Option(3))) // Some(())
   *     F.sequence_(List(Option(1), None, Option(3)))      // None
   * }}}
   */
  def sequence_[G[_]: Applicative, A, B](fga: F[G[A]]): G[Unit] =
    traverse_(fga)(identity)

  /**
   * Fold implemented using the given `MonoidK[G]` instance.
   *
   * This method is identical to fold, except that we use the universal monoid (`MonoidK[G]`)
   * to get a `Monoid[G[A]]` instance.
   *
   * For example:
   *
   * {{{
   *     val F = Foldable[List]
   *     F.foldK(List(1 :: 2 :: Nil, 3 :: 4 :: 5 :: Nil))
   *     // List(1, 2, 3, 4, 5)
   * }}}
   */
  def foldK[G[_], A](fga: F[G[A]])(implicit G: MonoidK[G]): G[A] =
    fold(fga)(G.algebra)


  /**
   * Find the first element matching the predicate, if one exists.
   */
  def find[A](fa: F[A])(f: A => Boolean): Option[A] =
    foldRight(fa, Lazy.eager(None: Option[A])) { a =>
      if (f(a)) Fold.Return(Some(a)) else Fold.Pass
    }.value


  /**
   * Convert F[A] to a List[A].
   */
  def toList[A](fa: F[A]): List[A] =
    foldLeft(fa, mutable.ListBuffer.empty[A]) { (buf, a) =>
      buf += a
    }.toList


  /**
   * Convert F[A] to a List[A], only including elements which match `p`.
   */
  def filter_[A](fa: F[A])(p: A => Boolean): List[A] =
    foldLeft(fa, mutable.ListBuffer.empty[A]) { (buf, a) =>
      if (p(a)) buf += a else buf
    }.toList


  /**
   * Convert F[A] to a List[A], dropping all initial elements which
   * match `p`.
   */
  def dropWhile_[A](fa: F[A])(p: A => Boolean): List[A] =
    foldLeft(fa, mutable.ListBuffer.empty[A]) { (buf, a) =>
      if (buf.nonEmpty || p(a)) buf += a else buf
    }.toList


  /**
   * Compose this `Foldable[F]` with a `Foldable[G]` to create
   * a `Foldable[F[G]]` instance.
   */
  def compose[G[_]](implicit G0: Foldable[G]): Foldable[λ[α => F[G[α]]]] =
    new CompositeFoldable[F, G] {
      implicit def F: Foldable[F] = self
      implicit def G: Foldable[G] = G0
    }
}

/**
 * Methods that apply to 2 nested Foldable instances
 */
trait CompositeFoldable[F[_], G[_]] extends Foldable[λ[α => F[G[α]]]] {
  implicit def F: Foldable[F]
  implicit def G: Foldable[G]

  /**
   * Left assocative fold on F[G[A]] using 'f'
   */
  def foldLeft[A, B](fga: F[G[A]], b: B)(f: (B, A) => B): B =
    F.foldLeft(fga, b)((b, a) => G.foldLeft(a, b)(f))

  /**
   * Right associative lazy fold on `F` using the folding function 'f'.
   */
  def partialFold[A, B](fga: F[G[A]])(f: A => Fold[B]): Fold[B] =
    F.partialFold(fga)(ga => G.partialFold(ga)(f))
}
